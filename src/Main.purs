module Main where

import Prelude
import Control.Monad.Aff (attempt, forkAff, joinFiber, launchAff_, makeAff, Aff, nonCanceler)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, throw)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.Blob (slice, size, type_, idxFromNumber, StartByte(..), EndByte(..))
import DOM.File.FileReader (result, fileReader)
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import DOM.File.FileReader as FileReader
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.XHR.FormData (toFormData, FormDataValue(..))
import DOM.XHR.Types (FormData)
import Data.Array as A
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.Bifunctor (rmap)
import Data.Either (either, Either(..), fromRight)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, unsafeReadTagged, unsafeFromForeign)
import Data.Foreign.Index (ix)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber, ceil, round, toStringAs, decimal)
import Data.List (List, filter, fromFoldable, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationOctetStream)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.String (null, joinWith)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, post, defaultRequest, retry, defaultRetryPolicy)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (responseHeaderName, responseHeaderValue)
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens, Lens')
import Pipes ((>->), await, yield)
import Pipes.Core (Consumer, Pipe, Producer, runEffect)
import Pipes.Aff (spawn, unbounded, toOutput, fromInput)

import Simple.JSON (class ReadForeign, read, readJSON')
{-- import Debug.Trace (traceAnyA) --}

foreign import sparkMD5Impl :: forall e. Fn1 ArrayBuffer (Eff e String)
sparkMD5 :: forall e. ArrayBuffer -> Eff e String
sparkMD5 ab = runFn1 sparkMD5Impl ab
--
-- File API helpers
--
readAs :: forall eff a. (Foreign -> F a) -> (Blob -> FileReader -> Eff (dom :: DOM | eff) Unit) -> Blob -> Aff (dom :: DOM | eff) a
readAs readMethod getResult blob = makeAff \cb -> do
  fr <- fileReader
  let et = fileReaderToEventTarget fr
  addEventListener EventTypes.error (eventListener \_ -> cb <<< Left $ error "FileReader error") false et
  addEventListener EventTypes.load (eventListener \_ -> do
      res <- result fr
      either
        (\errs -> cb <<< Left <<< error $ show errs)
        (cb <<< Right) $ runExcept $ readMethod res
    ) false et
  nonCanceler <$ getResult blob fr

readAsArrayBuffer :: forall eff. Blob -> Aff (dom :: DOM | eff) ArrayBuffer
readAsArrayBuffer = readAs (unsafeReadTagged "ArrayBuffer") FileReader.readAsArrayBuffer

--
--  Environment
--
newtype Config = Config
  { apikey :: String
  , partSize :: Int
  , storeTo :: StoreOptions
  }
derive instance ntC :: Newtype Config _
derive newtype instance rfC :: ReadForeign Config
instance showC :: Show Config where
  show (Config c) = [ "apikey: " <> c.apikey
                    , "partSize: " <> show c.partSize
                    , show c.storeTo
                    ] # joinWith "\n"

newtype StoreOptions = StoreOptions
  { location :: String
  , region :: NullOrUndefined String
  , container :: NullOrUndefined String
  , path :: NullOrUndefined String
  , access :: NullOrUndefined String
  }
derive instance ntSO :: Newtype StoreOptions _
derive newtype instance rfSO :: ReadForeign StoreOptions
instance showSO :: Show StoreOptions where
  show (StoreOptions so) = [ "store_location: " <> so.location
                           , "store_region: " <> show so.region
                           , "store_container: " <> show so.container
                           , "store_path: " <> show so.path
                           , "store_access: " <> show so.access
                           ] # joinWith "\n"

apikey :: forall a b r. Lens { apikey :: a | r } { apikey :: b | r } a b
apikey = lens _.apikey (_ { apikey = _ })
partSize :: forall a b r. Lens { partSize :: a | r } { partSize :: b | r } a b
partSize = lens _.partSize (_ { partSize = _ })
storeTo :: forall a b r. Lens { storeTo :: a | r } { storeTo :: b | r } a b
storeTo = lens _.storeTo (_ { storeTo = _ })
location :: forall a b r. Lens { location :: a | r } { location :: b | r } a b
location = lens _.location (_ { location = _ })

--
-- Upload types
--
newtype Part = Part
  { num :: Int
  , slice :: ArrayBuffer
  , md5 :: String
  , length :: Int
  }
partSlice :: Lens' Part ArrayBuffer
partSlice  = lens (\(Part x) -> x.slice) (\(Part x) v -> Part x { slice = v })
partNum :: Lens' Part Int
partNum  = lens (\(Part x) -> x.num) (\(Part x) v -> Part x { num = v })
partMD5 :: Lens' Part String
partMD5 = lens (\(Part x) -> x.md5) (\(Part x) v -> Part x { md5 = v })
partLength :: Lens' Part Int
partLength = lens (\(Part x) -> x.length) (\(Part x) v -> Part x { length = v })

instance showPart :: Show Part where
  show (Part p) =
    [ "num: " <> show p.num
    , "md5: " <> p.md5
    ] # joinWith "\n"

newtype Params = Params
  { location_url :: String
  , region :: String
  , upload_id :: String
  , uri :: String
  }
derive instance ntSP :: Newtype Params _
derive newtype instance rfP :: ReadForeign Params

type State =
  { params :: Params
  , file :: Blob
  , name :: String
  , cfg :: Config
  }

initialParams :: Params
initialParams = Params
  { location_url: ""
  , region: ""
  , upload_id: ""
  , uri: ""
  }

getFileType :: Blob -> MediaType
getFileType = fromMaybe applicationOctetStream <<< type_

mkPart :: State -> Int -> Producer (Tuple Part State) (Aff Effects) Unit
mkPart ctx n = do
  let ps = toNumber $ (unwrap ctx.cfg) ^. partSize
      totalSize = size ctx.file
      numParts = ceil $ totalSize / ps
      mediatype = getFileType ctx.file
      slice' = slice mediatype (StartByte startByte) (EndByte endByte)
      num = toNumber n
      startByte = idxFromNumber $ num * ps
      endByte = idxFromNumber $ min (num * ps + ps) totalSize
      blob = slice' ctx.file
      length = round $ size blob
  unless (n == numParts) do
    liftEff $ log $
      "Slicing part "
      <> show (n + 1)
      <> " of "
      <> show numParts
      <> " with length: "
      <> show length
    buf <- lift $ readAsArrayBuffer blob
    md5 <- liftEff $ runFn1 sparkMD5 buf
    yield $ Tuple (Part { num: n, slice: buf, length: length, md5: md5 }) ctx

-- Type for key/value pairs that we will convert to FormData
type Fields = List (Tuple String String)

getCommonFields :: State -> Aff Effects Fields
getCommonFields state = do
  let cfg = unwrap state.cfg
      so = unwrap (cfg ^. storeTo)
      sp = unwrap (state.params)
  pure $ fromFoldable [ Tuple "region" sp.region
                      , Tuple "upload_id" sp.upload_id
                      , Tuple "uri" sp.uri
                      , Tuple "location_url" sp.location_url
                      , Tuple "store_location" so.location
                      , Tuple "store_region" $ show $ unNullOrUndefined so.region
                      , Tuple "store_container" $ show $ unNullOrUndefined so.container
                      , Tuple "store_path" $ show $ unNullOrUndefined so.path
                      , Tuple "store_access" $ show $ unNullOrUndefined so.access
                      , Tuple "apikey" $ cfg ^. apikey
                      ] # filter (\(Tuple k v) -> null v == false && v /= "Nothing")

mkFormData :: Fields -> FormData
mkFormData ts = toFormData $ rmap FormDataString <$> ts

-- | Start the multipart upload flow
start :: State -> Blob -> Aff Effects (AffjaxResponse String)
start state file = do
  let fs = fromFoldable [ Tuple "mimetype" (show <<< unwrap $ getFileType file)
                        , Tuple "filename" state.name
                        , Tuple "size" (show <<< round $ size file)
                        ]
  common <- getCommonFields state
  post "https://upload.filestackapi.com/multipart/start" (mkFormData $ common <> fs)

getS3Data :: Pipe (Tuple Part State) (Tuple Part String) (Aff Effects) Unit
getS3Data = forever $ do
  (Tuple part state) <- await
  let fields = fromFoldable [ Tuple "size" (show $ part ^. partLength)
                            , Tuple "md5" (part ^. partMD5)
                            , Tuple "part" (toStringAs decimal $ (part ^. partNum) + 1)
                            ]
  common <- lift $ getCommonFields state
  res <- lift $ attempt $ retry defaultRetryPolicy affjax $ defaultRequest
    { url = "https://upload.filestackapi.com/multipart/upload"
    , method = Left POST
    , content = Just (mkFormData $ common <> fields)
    }
  case res of
    Left e -> lift $ liftEff $ throw "Failed to retrieve S3 metadata"
    Right r -> yield $ Tuple part r.response

newtype S3Params = S3Params
  { url :: String
  , headers :: Foreign
  , location_url :: String
  }
derive instance ntS3 :: Newtype S3Params _
derive newtype instance rfS3 :: ReadForeign S3Params

makeS3Headers :: Foreign -> Array RequestHeader
makeS3Headers ps = (\n -> RequestHeader n $ convert (runExcept $ ix ps n)) <$> ks
  where ks = either (const []) (\a -> a) (runExcept $ keys ps)
        convert = either (const "") (\v -> unsafeFromForeign v :: String)

getS3Req :: Pipe (Tuple Part String) (Tuple Part (Aff Effects (AffjaxResponse String))) (Aff Effects) Unit
getS3Req = forever $ do
  (Tuple part s3p) <- await
  let s3 = runExcept (readJSON' s3p :: F S3Params)
  case s3 of
    Left e ->
      liftEff $ log $ show e
    Right (S3Params params) -> do
      let hs = makeS3Headers params.headers
          buf = part ^. partSlice
          aff = retry defaultRetryPolicy affjax $ defaultRequest
            { url = params.url
            , headers = hs
            , method = Left PUT
            , content = Just (asUint8Array $ whole buf)
            }
      yield $ Tuple part aff

mkPartStr :: Int -> String -> String
mkPartStr n e = show (n + 1) <> ":" <> e

complete :: State -> Int -> Consumer (Tuple Int String) (Aff Effects) Unit
complete state total = go [] total
  where
    go tags n | n /= 0 = do
      (Tuple num etag) <- await
      go (tags <> [mkPartStr num etag]) (n - 1)
    go tags n = do
      let fields = fromFoldable [ Tuple "mimetype" (show <<< unwrap $ getFileType state.file)
                                , Tuple "filename" state.name
                                , Tuple "size" (show <<< round $ size $ state.file)
                                , Tuple "parts" $ tags # joinWith ";"
                                ]
      common <- lift $ getCommonFields state
      res <- lift $ attempt $ retry defaultRetryPolicy affjax $ defaultRequest
        { url = "https://upload.filestackapi.com/multipart/complete"
        , method = Left POST
        , content = Just (mkFormData $ common <> fields)
        }
      case res of
        Left e -> lift $ liftEff $ throw "Failed to complete S3 upload"
        Right r -> pure r.response

upload :: Blob -> State -> Aff Effects Unit
upload file ctx = do
  result <- start ctx file
  let sp = runExcept (readJSON' result.response)
      total = ceil $ (size file) / (toNumber ((unwrap ctx.cfg) ^. partSize))
  case sp of
    Left e ->
      liftEff $ throw "Start parameters could not be parsed"
    Right params -> do
      let context = ctx{ params = params :: Params }
      s3Channel <- spawn unbounded
      -- TODO Do this better somehow
      for_ (range 0 (total - 1)) \x -> do
        forkAff $ runEffect $ mkPart context x
          >-> getS3Data
          >-> getS3Req
          >-> s3Scheduler
          >-> toOutput s3Channel
      runEffect $ fromInput s3Channel >-> complete context total
      pure unit

s3Scheduler :: Pipe (Tuple Part (Aff Effects (AffjaxResponse String))) (Tuple Int String) (Aff Effects) Unit
s3Scheduler = forever $ do
  (Tuple part aff) <- await
  -- TODO Throttle these requests
  fiber <- lift $ forkAff aff
  res <- lift $ joinFiber fiber
  let etagHeader = res.headers # A.filter (\s -> responseHeaderName s == "etag")
      etag = (responseHeaderValue <$> etagHeader) # joinWith ""
  yield (Tuple (part ^. partNum) etag)

type Effects =
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , dom :: DOM
  , exception :: EXCEPTION
  , ref :: REF
  )
main :: Partial => Blob -> String -> Foreign -> Eff Effects Unit
main file name cfg = launchAff_ $ upload file init
  where init = { params: initialParams, file: file, name: name, cfg: cfg' }
        cfg' = fromRight $ runExcept (read cfg)
