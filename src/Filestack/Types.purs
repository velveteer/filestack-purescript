module Filestack.Types
  ( Config(..)
  , Params(..)
  , Part(..)
  , StoreOptions(..)
  , Effects
  , State

  -- Optics
  , apikey
  , initialParams
  , location
  , partSize

  , partLength
  , partNum
  , partSlice
  , partMD5
  , storeTo
  ) where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Worker (WORKER)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import DOM (DOM)
import DOM.File.Types (Blob)
import Network.HTTP.Affjax (AJAX)
import Optic.Lens (lens)
import Optic.Types (Lens, Lens')
import Simple.JSON (class ReadForeign)

showN :: NullOrUndefined String -> String
showN = show <<< unNullOrUndefined

newtype Config = Config
  { apikey :: String
  , partSize :: Int
  , concurrency :: Int
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
  , region :: String
  , container :: String
  , path :: String
  , access :: String
  }

derive instance ntSO :: Newtype StoreOptions _
derive newtype instance rfSO :: ReadForeign StoreOptions
instance showSO :: Show StoreOptions where
  show (StoreOptions so) = [ "store_location: " <> (so.location)
                           , "store_region: " <> (so.region)
                           , "store_container: " <> (so.container)
                           , "store_path: " <> (so.path)
                           , "store_access: " <> (so.access)
                           ] # joinWith "\n"

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
  , md5 :: String
  , length :: Int
  }
partSlice :: Lens' Part Blob
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
  , partNum :: Int
  }

initialParams :: Params
initialParams = Params
  { location_url: ""
  , region: ""
  , upload_id: ""
  , uri: ""
  }

type Effects =
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , dom :: DOM
  , exception :: EXCEPTION
  , ref :: REF
  , worker :: WORKER
  )
