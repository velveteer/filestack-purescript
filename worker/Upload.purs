module Upload
where

import Prelude
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
{-- import Data.ArrayBuffer.DataView (whole) --}
{-- import Data.ArrayBuffer.Typed (asUint8Array) --}
import Data.Int (toNumber, round, toStringAs, decimal)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, unsafeFromForeign)
import Data.Foreign.Index (ix)
import Data.Foreign.Keys (keys)
import Data.Function.Uncurried (runFn1)
import Data.HTTP.Method (Method(..))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (toLower, joinWith)
import Data.Tuple (Tuple(..))
import DOM.File.Blob (slice, size, idxFromNumber, StartByte(..), EndByte(..))
import GlobalScope.Dedicated (onMessage, postMessage)
import Network.HTTP.Affjax (AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (responseHeaderName, responseHeaderValue)
import Optic.Getter ((^.))
import Pipes ((>->), yield, await)
import Pipes.Core (Consumer, Pipe, Producer, runEffect)
import Pipes.Prelude (drain)
import Simple.JSON (class ReadForeign, readJSON')
{-- import Debug.Trace (traceAnyA) --}

import Filestack.Types
  ( Effects
  , Part(..)
  , State
  , partLength
  , partNum
  , partMD5
  , partSize
  , partSlice
  )
import Filestack.Utils
  ( getCommonFields
  , getFileType
  , mkFormData
  , readAsArrayBuffer
  , sparkMD5
  )

mkPart :: State -> Producer (Tuple Part State) (Aff Effects) Unit
mkPart ctx = do
  let n = ctx.partNum
      ps = toNumber $ (unwrap ctx.cfg) ^. partSize
      totalSize = size ctx.file
      mediatype = getFileType ctx.file
      slice' = slice mediatype (StartByte startByte) (EndByte endByte)
      num = toNumber n
      startByte = idxFromNumber $ num * ps
      endByte = idxFromNumber $ min (num * ps + ps) totalSize
      blob = slice' ctx.file
      length = round $ size blob
  buf <- lift $ readAsArrayBuffer blob
  md5 <- liftEff $ runFn1 sparkMD5 buf
  yield $ Tuple (Part { num: n, slice: blob, length: length, md5: md5 }) ctx

getS3Data :: Pipe (Tuple Part State) (Tuple Part String) (Aff Effects) Unit
getS3Data = do
  (Tuple part state) <- await
  let fields = fromFoldable [ Tuple "size" (show $ part ^. partLength)
                            , Tuple "md5" (part ^. partMD5)
                            , Tuple "part" (toStringAs decimal $ (part ^. partNum) + 1)
                            ] <> getCommonFields state
  res <- lift $ attempt $ affjax $ defaultRequest
    { url = "https://upload.filestackapi.com/multipart/upload"
    , method = Left POST
    , content = Just (mkFormData fields)
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

getEtag :: AffjaxResponse String -> String
getEtag res = (responseHeaderValue <$> etagHeader) # joinWith ""
  where etagHeader = res.headers # A.filter (\s -> toLower (responseHeaderName s) == "etag")

makeS3Req :: Pipe (Tuple Part String) String (Aff Effects) Unit
makeS3Req = do
  (Tuple part s3p) <- await
  let s3 = runExcept (readJSON' s3p :: F S3Params)
  case s3 of
    Left e -> liftEff $ throw $ show e
    Right (S3Params params) -> do
      let hs = makeS3Headers params.headers
          aff = affjax $ defaultRequest
            { url = params.url
            , headers = hs
            , method = Left PUT
            , content = Just (part ^. partSlice)
            }
      res <- lift $ attempt aff
      case res of
        Left e -> liftEff $ throw (show e)
        Right res' -> do
          yield $ "" <> (show $ part ^. partNum + 1) <> ":" <> (getEtag res')

uploadWorker :: State -> (Eff Effects) Unit
uploadWorker ctx = do
  launchAff_ $ runEffect $ mkPart ctx
    >-> getS3Data
    >-> makeS3Req
    >-> send
    >-> drain

send :: Consumer String (Aff Effects) Unit
send = do
  res <- await
  lift $ liftEff $ postMessage res

main :: Eff Effects Unit
main = do
  log "worker loaded"
  onMessage (\m -> uploadWorker m)
