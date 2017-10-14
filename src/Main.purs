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
import Control.Monad.Writer (tell)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.Blob (slice, size, type_, idxFromNumber, StartByte(..), EndByte(..))
import DOM.File.FileReader (result, fileReader)
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import DOM.File.FileReader as FileReader
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.XHR.FormData (toFormData, FormDataValue(..))
import Data.Array (filter, range)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Either (either, hush, Either(..), fromRight)
import Data.Foreign (F, Foreign, unsafeReadTagged, readString)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types as GT
import Data.Foreign.Class (class Decode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber, ceil, round, toStringAs, decimal)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationOctetStream)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))

import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash (base64, Algorithm(..))
import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens, Lens')

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
derive instance genericConfig :: Rep.Generic Config _
instance showConfig :: Show Config where
  show = genericShow

newtype StoreOptions = StoreOptions
  { location :: String
  , region :: NullOrUndefined String
  , container :: NullOrUndefined String
  , path :: NullOrUndefined String
  , access :: NullOrUndefined String
  }
derive instance genericStoreOptions :: Rep.Generic StoreOptions _
instance showStoreOptions :: Show StoreOptions where
  show = genericShow
instance decodeStoreOptions :: Decode StoreOptions where
  decode = genericDecode decodeOpts

type Log = Array String

newtype State = State { sParams :: Maybe StartParams }
derive instance genericState :: Rep.Generic State _
instance showState :: Show State where
  show = genericShow

type Env = RWST Config Log State

--
--  Lenses
--
_Config :: forall f.
  Functor f
  => ( { apikey :: String , partSize :: Int , storeTo :: StoreOptions }
  -> f { apikey :: String , partSize :: Int , storeTo :: StoreOptions } )
  -> Config
  -> f Config
_Config = lens (\(Config x) -> x) (const Config)

_StoreOptions :: forall f.
  Functor f
  => ( { location :: String
       , region :: NullOrUndefined String
       , container :: NullOrUndefined String
       , path :: NullOrUndefined String
       , access :: NullOrUndefined String
      }
  -> f { location :: String
       , region :: NullOrUndefined String
       , container :: NullOrUndefined String
       , path :: NullOrUndefined String
       , access :: NullOrUndefined String
       }
     )
  -> StoreOptions
  -> f StoreOptions
_StoreOptions = lens (\(StoreOptions x) -> x) (const StoreOptions)

_Part :: forall f.
  Functor f
  => ( { num :: Int , slice :: Blob }
  -> f { num :: Int , slice :: Blob } )
  -> Part
  -> f Part
_Part = lens (\(Part x) -> x) (const Part)

_State :: forall f.
  Functor f
  => ( { sParams :: Maybe StartParams }
  -> f { sParams :: Maybe StartParams } )
  -> State
  -> f State
_State = lens (\(State x) -> x) (const State)

_storeOpts :: forall f. Functor f
  => ( { location :: String
       , region :: NullOrUndefined String
       , container :: NullOrUndefined String
       , path :: NullOrUndefined String
       , access :: NullOrUndefined String
      }
  -> f { location :: String
       , region :: NullOrUndefined String
       , container :: NullOrUndefined String
       , path :: NullOrUndefined String
       , access :: NullOrUndefined String
       }
     )
  -> Config
  -> f Config
_storeOpts = _Config..storeTo.._StoreOptions

apikey :: forall a b r. Lens { apikey :: a | r } { apikey :: b | r } a b
apikey = lens _.apikey (_ { apikey = _ })
partSize :: forall a b r. Lens { partSize :: a | r } { partSize :: b | r } a b
partSize = lens _.partSize (_ { partSize = _ })
storeTo :: forall a b r. Lens { storeTo :: a | r } { storeTo :: b | r } a b
storeTo = lens _.storeTo (_ { storeTo = _ })
location :: forall a b r. Lens { location :: a | r } { location :: b | r } a b
location = lens _.location (_ { location = _ })
sParams :: forall a b r. Lens { sParams :: a | r } { sParams :: b | r } a b
sParams = lens _.sParams (_ { sParams = _ })

--
-- Upload types
--
newtype Part = Part
  { num :: Int
  , slice :: Blob
  }
partSlice :: Lens' Part Blob
partSlice  = lens (\(Part x) -> x.slice) (\(Part x) v -> Part x { slice = v })
partNum :: Lens' Part Int
partNum  = lens (\(Part x) -> x.num) (\(Part x) v -> Part x { num = v })

newtype StartParams = StartParams
  { location_url :: String
  , region :: String
  , upload_id :: String
  , uri :: String
  }
derive instance genericSP :: Rep.Generic StartParams _
instance showSP :: Show StartParams where
  show = genericShow

getFileType :: Blob -> String
getFileType = unwrap <<< fromMaybe applicationOctetStream <<< type_

decodeOpts :: GT.Options
decodeOpts = defaultOptions { unwrapSingleConstructors = true }

mkPart :: Blob -> Number -> Int -> Part
mkPart file ps p = Part{ num: p, slice: slice' file }
  where slice' = slice (wrap $ getFileType file) (StartByte startByte) (EndByte endByte)
        p' = toNumber p
        total = size file
        startByte = idxFromNumber $ p' * ps
        endByte = idxFromNumber $ min (p' * ps + ps) total

getCommonHeaders :: Maybe StartParams -> Array (Tuple String FormDataValue)
getCommonHeaders Nothing = []
getCommonHeaders (Just (StartParams sp)) = rmap FormDataString <$>
  [ Tuple "region" sp.region
  , Tuple "upload_id" sp.upload_id
  , Tuple "uri" sp.uri
  , Tuple "location_url" sp.location_url
  ]

-- |
-- Start the multipart upload flow
start :: Blob -> Env (Aff Effects) (AffjaxResponse Foreign)
start file = do
  cfg <- ask
  let fields = [ Tuple "apikey" (FormDataString $ cfg ^. _Config..apikey)
               , Tuple "mimetype" (FormDataString <<< show $ getFileType file)
               , Tuple "filename" (FormDataString "testfile") -- TODO getFilename
               , Tuple "size" (FormDataString <<< show <<< round $ size file)
               , Tuple "store_location" (FormDataString $ cfg ^. _storeOpts..location)
               ]
      fd = toFormData fields
  lift $ post "https://upload.filestackapi.com/multipart/start" fd

{-- newtype S3Params = S3Params --}
{--   { url :: String --}
{--   , location_url :: String --}
{--   , headers :: Foreign --}
{--   } --}

getS3Data :: Part -> Env (Aff Effects) (AffjaxResponse Foreign)
getS3Data part = do
  cfg <- ask
  st <- get
  text <- lift $ readAsText $ part ^. partSlice
  md5 <- liftEff $ base64 MD5 text
  let fields = (getCommonHeaders $ st ^. _State..sParams) <>
               [ Tuple "size" (FormDataString <<< show <<< round <<< size $ part ^. partSlice)
               , Tuple "md5" (FormDataString md5)
               , Tuple "part" (FormDataString $ toStringAs decimal $ (part ^. partNum) + 1)
               , Tuple "store_location" (FormDataString $ cfg ^. _storeOpts..location)
               , Tuple "apikey" (FormDataString $ cfg ^. _Config..apikey)
               ]
      fd = toFormData fields
  lift $ post "https://upload.filestackapi.com/multipart/upload" fd

upload :: Blob -> Env (Aff Effects) Unit
upload file = do
  cfg <- ask
  lift <<< liftEff <<< log $ show cfg
  let total = size file
      ps = toNumber $ cfg ^. _Config..partSize
      numParts = ceil $ total / ps
      parts = mkPart file ps
        <$> range 0 numParts
          # filter \(Part p) -> (round $ size p.slice) > 0
  result <- start file
  let sp = hush $ runExcept (genericDecode decodeOpts result.response)
  put $ State{ sParams: sp }
  get >>= \n -> tell [ "Received start parameters: " <> show n]
  s3Data <- getS3Data $ Part { num: 1, slice: file } -- TODO Map this over parts
  pure unit

type Effects = ( ajax :: AJAX, console :: CONSOLE , dom :: DOM , buffer :: BUFFER , crypto :: CRYPTO)
main :: Partial => Blob -> Foreign -> Env (Eff Effects) Unit
main file cfg = lift $ launchAff_ $ runRWST (upload file) cfg' $ State{ sParams: Nothing }
  where cfg' = fromRight $ runExcept (genericDecode decodeOpts cfg)
