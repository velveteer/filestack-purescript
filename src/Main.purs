module Main where

import Prelude
import Control.Monad.Aff (launchAff_, makeAff, Aff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.RWS.Trans (RWST, runRWST)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.Blob (slice, size, type_, idxFromNumber, StartByte(..), EndByte(..))
import DOM.File.FileReader (result, fileReader)
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import DOM.File.FileReader as FileReader
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.XHR.FormData (toFormData, FormDataValue(..))
import DOM.XHR.Types (FormData)
import Data.Array (filter, range)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Either (either, Either(..), fromRight)
import Data.Foreign (F, Foreign, unsafeReadTagged, readString)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Int (toNumber, ceil, round, toStringAs, decimal)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.MediaType.Common (applicationOctetStream)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.String (null, joinWith)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash (base64, Algorithm(..))
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens, Lens')
import Simple.JSON (class ReadForeign, read)
import Debug.Trace (traceAnyA)

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

readAsText :: forall eff. Blob -> Aff (dom :: DOM | eff) String
readAsText = readAs readString FileReader.readAsText

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

type Log = Array String
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
  , slice :: Blob
  , md5 :: String
  }
partSlice :: Lens' Part Blob
partSlice  = lens (\(Part x) -> x.slice) (\(Part x) v -> Part x { slice = v })
partNum :: Lens' Part Int
partNum  = lens (\(Part x) -> x.num) (\(Part x) v -> Part x { num = v })
partMD5 :: Lens' Part String
partMD5 = lens (\(Part x) -> x.md5) (\(Part x) v -> Part x { md5 = v })

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

mkPart :: Blob -> Number -> Int -> Aff Effects (Maybe Part)
mkPart file ps p = do
  let slice' = slice (getFileType file) (StartByte startByte) (EndByte endByte)
      p' = toNumber p
      total = size file
      startByte = idxFromNumber $ p' * ps
      endByte = idxFromNumber $ min (p' * ps + ps) total
      blob = slice' file
  case (round $ size blob) > 0 of
    false -> pure Nothing
    true -> do
      text <- readAsText blob
      md5 <- liftEff $ base64 MD5 text
      pure $ Just $ Part{ num: p, slice: slice' file, md5: md5 }

getCommonFormData :: Env (Aff Effects) (Array (Tuple String String))
getCommonFormData = do
  Config cfg <- ask
  Params sp <- get
  let so = unwrap (cfg ^. storeTo)
  pure $ [ Tuple "region" sp.region
         , Tuple "upload_id" sp.upload_id
         , Tuple "uri" sp.uri
         , Tuple "location_url" sp.location_url
         , Tuple "store_location" so.location
         , Tuple "apikey" $ cfg ^. apikey
         ] # filter (\(Tuple k v) -> null v == false)

mkFormData :: Array (Tuple String String) -> Env (Aff Effects) FormData
mkFormData ts = do
  base <- getCommonFormData
  pure $ toFormData $ rmap FormDataString <$> ts <> base

-- | Start the multipart upload flow
start :: Blob -> Env (Aff Effects) (AffjaxResponse Foreign)
start file = do
  let fields = [ Tuple "mimetype" (show <<< unwrap $ getFileType file)
               , Tuple "filename" "testfile" -- TODO getFilename
               , Tuple "size" (show <<< round $ size file)
               ]
  fd <- mkFormData fields
  lift $ post "https://upload.filestackapi.com/multipart/start" fd

{-- newtype S3Params = S3Params --}
{--   { url :: String --}
{--   , location_url :: String --}
{--   , formdata :: Foreign --}
{--   } --}

getS3Data :: Part -> Env (Aff Effects) (AffjaxResponse Foreign)
getS3Data part = do
  let fields = [ Tuple "size" (show <<< round <<< size $ part ^. partSlice)
               , Tuple "md5" (part ^. partMD5)
               , Tuple "part" (toStringAs decimal $ (part ^. partNum) + 1)
               ]
  fd <- mkFormData fields
  lift $ post "https://upload.filestackapi.com/multipart/upload" fd

upload :: Blob -> Env (Aff Effects) Unit
upload file = do
  cfg <- ask
  lift $ liftEff $ log $ show cfg
  let total = size file
      ps = toNumber $ (unwrap cfg) ^. partSize
      numParts = ceil $ total / ps
  parts <- lift $ parTraverse (mkPart file ps) $ range 0 numParts
  let validParts = parts # filter isJust
  traceAnyA validParts
  {-- result <- start file --}
  {-- let sp = runExcept (read result.response) --}
  {-- case sp of --}
  {--   Left e -> --}
  {--     liftEff $ log "Start parameters could not be parsed" --}
  {--   Right params -> --}
  {--     put params --}
  {-- s3Data <- parTraverse getS3Data parts --}
  pure unit

type Effects = ( ajax :: AJAX, console :: CONSOLE , dom :: DOM , buffer :: BUFFER , crypto :: CRYPTO)
main :: Partial => Blob -> Foreign -> Env (Eff Effects) Unit
main file cfg = lift $ launchAff_ $ runRWST app cfg' state
  where app = upload file
        cfg' = fromRight $ runExcept (read cfg)
        state = initialParams
