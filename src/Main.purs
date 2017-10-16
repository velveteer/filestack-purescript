module Main where

import Prelude
import Control.Monad.Aff (attempt, forkAff, joinFiber, launchAff_, makeAff, Aff, Fiber, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throw)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.RWS.Trans (RWST, runRWST)
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
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.Bifunctor (rmap)
import Data.Either (either, Either(..), fromRight)
import Data.Foreign (F, Foreign, unsafeReadTagged, unsafeFromForeign)
import Data.Foreign.Index (ix)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber, ceil, round, toStringAs, decimal)
import Data.List (List, filter, range, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationOctetStream)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.String (null, joinWith)
import Data.Tuple (Tuple(..), uncurry, fst, snd)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, post, defaultRequest, retry, defaultRetryPolicy)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (responseHeaderName, responseHeaderValue)
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens, Lens')
import Pipes ((>->), await, each, yield)
import Pipes.Core (Pipe, runEffect)
import Pipes.Prelude as P
import Simple.JSON (class ReadForeign, read, readJSON')
import Debug.Trace (traceAnyA)

foreign import sparkMD5Impl :: Fn1 ArrayBuffer String
sparkMD5 :: ArrayBuffer -> String
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

type Log = List String
type Env = RWST Config Log Params

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
  }
partSlice :: Lens' Part ArrayBuffer
partSlice  = lens (\(Part x) -> x.slice) (\(Part x) v -> Part x { slice = v })
partNum :: Lens' Part Int
partNum  = lens (\(Part x) -> x.num) (\(Part x) v -> Part x { num = v })
partMD5 :: Lens' Part String
partMD5 = lens (\(Part x) -> x.md5) (\(Part x) v -> Part x { md5 = v })

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

initialParams :: Params
initialParams = Params
  { location_url: ""
  , region: ""
  , upload_id: ""
  , uri: ""
  }

getFileType :: Blob -> MediaType
getFileType = fromMaybe applicationOctetStream <<< type_

mkPart :: Blob -> Number -> Int -> Aff Effects Part
mkPart file ps p = do
  let slice' = slice (getFileType file) (StartByte startByte) (EndByte endByte)
      p' = toNumber p
      total = size file
      startByte = idxFromNumber $ p' * ps
      endByte = idxFromNumber $ min (p' * ps + ps) total
      blob = slice' file
  liftEff $ log $ "Slicing part " <> show p
  buf <- readAsArrayBuffer blob
  let md5 = runFn1 sparkMD5 buf
  pure $ Part{ num: p, slice: buf, md5: md5 }

-- Type for key/value pairs that we will convert to FormData
type Fields = List (Tuple String String)

getCommonFields :: Env (Aff Effects) Fields
getCommonFields = do
  Config cfg <- ask
  Params sp <- get
  let so = unwrap (cfg ^. storeTo)
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
start :: Blob -> Env (Aff Effects) (AffjaxResponse String)
start file = do
  let fs = fromFoldable [ Tuple "mimetype" (show <<< unwrap $ getFileType file)
                        , Tuple "filename" "testfile" -- TODO getFilename
                        , Tuple "size" (show <<< round $ size file)
                        ]
  common <- getCommonFields
  lift $ post "https://upload.filestackapi.com/multipart/start" (mkFormData $ common <> fs)

getS3Data :: Fields -> Part -> Aff Effects (Tuple String Part)
getS3Data form part = do
  let fields = form <> fromFoldable [ Tuple "size" (show <<< byteLength $ part ^. partSlice)
                                    , Tuple "md5" (part ^. partMD5)
                                    , Tuple "part" (toStringAs decimal $ (part ^. partNum) + 1)
                                    ]
  res <- attempt $ retry defaultRetryPolicy affjax $ defaultRequest
    { url = "https://upload.filestackapi.com/multipart/upload"
    , method = Left POST
    , content = Just (mkFormData fields)
    }
  case res of
    Left e -> liftEff $ throw "Failed to retrieve S3 metadata"
    Right r -> pure $ Tuple r.response part

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

uploadToS3 :: String -> Part -> Aff Effects (Tuple Part (AffjaxResponse String))
uploadToS3 s3p part = do
  let s3 = runExcept (readJSON' s3p :: F S3Params)
  case s3 of
    Left e ->
      liftEff $ throw $ show e
    Right (S3Params params) -> do
      let hs = makeS3Headers params.headers
          bytes = part ^. partSlice

      res <- retry defaultRetryPolicy affjax $ defaultRequest
        { url = params.url
        , headers = hs
        , method = Left PUT
        , content = Just (asUint8Array $ whole bytes)
        }
      pure $ Tuple part res

upload :: Partial => Blob -> Env (Aff Effects) Unit
upload file = do
  cfg <- ask
  lift $ liftEff $ log $ show cfg
  let total = size file
      ps = toNumber $ (unwrap cfg) ^. partSize
      numParts = ceil $ total / ps
      partAffs = mkPart file ps <$> range 0 (numParts - 1)

  -- Call /multipart/start and store returned params in State monad
  result <- start file
  let sp = runExcept (readJSON' result.response)
  case sp of
    Left e ->
      lift $ liftEff $ throw "Start parameters could not be parsed"
    Right params ->
      put params
  fields <- getCommonFields
  traceAnyA partAffs
  lift $ runEffect $ each partAffs
    >-> P.mapM forkAff
    >-> P.mapM joinFiber
    >-> P.mapM (getS3Data fields)
    >-> P.map (uncurry uploadToS3)
    >-> register
    >-> P.drain
  pure unit

register :: Pipe (Aff Effects (Tuple Part (AffjaxResponse String))) Foreign (Aff Effects) Unit
register = do
  t <- await
  fiber <- lift $ forkAff t
  pair <- lift $ joinFiber fiber
  let part = fst pair
      req = snd pair
  traceAnyA part
  traceAnyA req
  {-- fiber' <- lift $ forkAff req --}
  {-- res <- lift $ joinFiber fiber' --}
  {-- let etagHeader = res.headers # A.filter (\s -> responseHeaderName s == "etag") --}
  {-- traceAnyA $ (responseHeaderValue <$> etagHeader) # joinWith "" --}

type Effects =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM
  , exception :: EXCEPTION
  , ref :: REF
  )
main :: Partial => Blob -> Foreign -> Env (Eff Effects) Unit
main file cfg = lift $ launchAff_ $ runRWST app cfg' state
  where app = upload file
        cfg' = fromRight $ runExcept (read cfg)
        state = initialParams
