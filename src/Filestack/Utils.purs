module Filestack.Utils
  ( getCommonFields
  , getFileType
  , mkFormData
  , readAsArrayBuffer
  , sparkMD5
  )
where

import Prelude
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Either (either, Either(..))
import Data.Foreign (F, Foreign, unsafeReadTagged)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.List (List, filter, fromFoldable)
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationOctetStream)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (Tuple(..))
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.Blob (type_)
import DOM.File.FileReader (result, fileReader)
import DOM.File.FileReader as FileReader
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.XHR.FormData (toFormData, FormDataValue(..))
import DOM.XHR.Types (FormData)
import Optic.Getter ((^.))

import Filestack.Types (State, apikey, storeTo)

foreign import sparkMD5Impl :: forall e. Fn1 ArrayBuffer (Eff e String)
sparkMD5 :: forall e. ArrayBuffer -> Eff e String
sparkMD5 ab = runFn1 sparkMD5Impl ab

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


getFileType :: Blob -> MediaType
getFileType = fromMaybe applicationOctetStream <<< type_

-- Type for key/value pairs that we will convert to FormData
type Fields = List (Tuple String String)

getDefault :: String -> NullOrUndefined String -> String
getDefault d = fromMaybe d <<< unNullOrUndefined

getCommonFields :: State -> Fields
getCommonFields state = do
  let cfg = unwrap state.cfg
      so = unwrap (cfg ^. storeTo)
      sp = unwrap (state.params)
      base = fromFoldable
        [ Tuple "region" sp.region
        , Tuple "upload_id" sp.upload_id
        , Tuple "uri" sp.uri
        , Tuple "location_url" sp.location_url
        , Tuple "apikey" $ cfg ^. apikey
        ] # filter (\(Tuple k v) -> null v /= true)
  base <> fromFoldable
      [ Tuple "store_location" so.location
      , Tuple "store_region" so.region
      , Tuple "store_container" so.container
      , Tuple "store_path" so.path
      , Tuple "store_access" so.access
      ] # filter (\(Tuple k v) -> null v /= true)

mkFormData :: Fields -> FormData
mkFormData ts = toFormData $ rmap FormDataString <$> ts
