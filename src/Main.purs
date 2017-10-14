module Main where

import Prelude
import DOM.File.FileReader as FileReader
import DOM.HTML.Event.EventTypes as EventTypes
import Control.Monad.Aff (attempt, forkAff, launchAff_, makeAff, Aff, Fiber, joinFiber, runAff, nonCanceler)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error, try)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.RWS.Trans (RWST, RWSResult, runRWST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.Blob (slice, size, type_, idxFromNumber, StartByte(..), EndByte(..))
import DOM.File.FileReader (result, fileReader)
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import DOM.XHR.FormData (toFormData, FormDataValue(..))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (either, hush, Either(..))
import Data.Foreign (F, Foreign, unsafeReadTagged, readString)
import Data.Int (toNumber, ceil, round)
import Data.Generic (class Generic, gShow)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Array (filter, range, (!!))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationOctetStream)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAnyA)

import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
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

newtype StoreOptions = StoreOptions
  { location :: String
  , region :: Maybe String
  , container :: Maybe String
  , path :: Maybe String
  , access :: Maybe String
  }
derive instance genericStoreOptions :: Rep.Generic StoreOptions _
instance showStoreOptions :: Show StoreOptions where
  show = genericShow

newtype Config = Config
  { apikey :: String
  , partSize :: Int
  , storeTo :: StoreOptions
  }
derive instance genericConfig :: Rep.Generic Config _
instance showConfig :: Show Config where
  show = genericShow

type Log = Array String
data State = State
  { sParams :: Maybe StartParams
  }
type Env = RWST Config Log State

_Config ::
  Lens Config
  Config
  { apikey :: String, partSize :: Int, storeTo :: StoreOptions }
  { apikey :: String, partSize :: Int, storeTo :: StoreOptions }
_Config = lens (\(Config x) -> x) (const Config)

_StoreOptions = lens (\(StoreOptions x) -> x) (const StoreOptions)

apikey :: forall a b r. Lens { apikey :: a | r } { apikey :: b | r } a b
apikey = lens _.apikey (_ { apikey = _ })
partSize :: forall a b r. Lens { partSize :: a | r } { partSize :: b | r } a b
partSize = lens _.partSize (_ { partSize = _ })
storeTo :: forall a b r. Lens { storeTo :: a | r } { storeTo :: b | r } a b
storeTo = lens _.storeTo (_ { storeTo = _ })
location :: forall a b r. Lens { location :: a | r } { location :: b | r } a b
location = lens _.location (_ { location = _ })

newtype Part = Part
  { num :: Int
  , slice :: Blob
  }
partSlice :: Lens' Part Blob
partSlice  = lens (\(Part x) -> x.slice) (\(Part x) v -> Part x { slice = v })

newtype StartParams = StartParams
  { location_url :: String
  , region :: String
  , upload_id :: String
  , uri :: String
  }
derive instance genericSP :: Rep.Generic StartParams _
instance showSP :: Show StartParams where
  show = genericShow

start :: forall e. Blob -> Env (Aff ( ajax :: AJAX | e )) (AffjaxResponse Foreign)
start file = do
  cfg <- ask
  let fields = [ Tuple "apikey" (FormDataString $ cfg ^. _Config..apikey)
               , Tuple "mimetype" (FormDataString <<< show $ getFileType file)
               , Tuple "filename" (FormDataString "testfile")
               , Tuple "size" (FormDataString <<< show <<< round $ size file)
               , Tuple "store_location" (FormDataString $ cfg ^. _Config..storeTo.._StoreOptions..location)
               ]
      fd = toFormData fields
  lift $ post "https://upload.filestackapi.com/multipart/start" fd

{-- getS3Data :: forall e. Part -> Env (Eff ( ajax :: AJAX | e )) (Fiber ( ajax :: AJAX | e) (AffjaxResponse Foreign)) --}
{-- getS3Data part = do --}
{--   cfg <- ask --}
{--   let s = cfg ^. sParams --}
{--       fields = [ Tuple "apikey" (FormDataString $ cfg ^. apikey) --}
{--                , Tuple "size" (FormDataString <<< show <<< round <<< size $ part ^. partSlice) --}
{--                , Tuple "store_location" (FormDataString $ cfg ^. storeTo..location) --}
{--                ] --}
{--       fd = toFormData fields --}
{--   lift <<< launchAff $ post "https://upload.filestackapi.com/multipart/start" fd --}

getFileType :: Blob -> String
getFileType = unwrap <<< fromMaybe applicationOctetStream <<< type_

mkPart :: Blob -> Number -> Int -> Part
mkPart file ps p = Part{ num: p, slice: slice' file }
  where slice' = slice (wrap $ getFileType file) (StartByte startByte) (EndByte endByte)
        p' = toNumber p
        total = size file
        startByte = idxFromNumber $ p' * ps
        endByte = idxFromNumber $ min (p' * ps + ps) total

opts = defaultOptions { unwrapSingleConstructors = true }

upload :: forall e. Blob -> Env (Aff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | e )) Unit
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
  let sp = hush $ runExcept (genericDecode opts result.response)
  put $ State{ sParams: sp }

main :: forall e. Blob -> Config -> Env (Eff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | e )) Unit
main file cfg = lift $ launchAff_ $ runRWST (upload file) cfg $ State{ sParams: Nothing }
