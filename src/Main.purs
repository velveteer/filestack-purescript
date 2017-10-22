module Main where

import Prelude
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Worker.Master (sendMessage, onMessage, startWorker)
import Data.Either (Either(..), fromRight)
import Data.Foldable (for_)
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber, ceil, round)
import Data.List (fromFoldable, range)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DOM.File.Blob (size)
import DOM.File.Types (Blob)
import Network.HTTP.Affjax (AffjaxResponse, affjax, post, defaultRequest, retry, defaultRetryPolicy)
import Optic.Getter ((^.))
import Pipes ((>->), await)
import Pipes.Core (Consumer, runEffect)
import Pipes.Aff (send, spawn, unbounded, fromInput)
import Simple.JSON (read, readJSON')
{-- import Debug.Trace (traceAnyA) --}

import Filestack.Utils (getFileType, getCommonFields, mkFormData)
import Filestack.Types (Effects, Params, State, initialParams, partSize)
import Filestack.Upload (workerModule)

-- | Start the multipart upload flow
start :: State -> Blob -> Aff Effects (AffjaxResponse String)
start state file = do
  let fs = fromFoldable [ Tuple "mimetype" (show <<< unwrap $ getFileType file)
                        , Tuple "filename" state.name
                        , Tuple "size" (show <<< round $ size file)
                        ] <> getCommonFields state
  post "https://upload.filestackapi.com/multipart/start" (mkFormData fs)

upload :: Blob -> State -> Aff Effects Unit
upload file ctx = do
  result <- start ctx file
  let sp = runExcept (readJSON' result.response)
      cfg = unwrap ctx.cfg
      total = ceil $ (size file) / (toNumber (cfg ^. partSize))
  case sp of
    Left e ->
      liftEff $ throw "Start parameters could not be parsed"
    Right params -> do
      let context = ctx{ params = params :: Params }
      s3Channel <- spawn unbounded
      for_ (range 0 (total - 1)) \x -> do
        let newCtx = context{ partNum = x }
        w1 <- liftEff $ startWorker workerModule
        liftEff $ sendMessage w1 newCtx
        liftEff $ onMessage w1 (\m -> launchAff_ (send m s3Channel))
      runEffect $ fromInput s3Channel >-> complete context total
      pure unit

complete :: State -> Int -> Consumer String (Aff Effects) Unit
complete state total = go [] total
  where
    go tags n | n /= 0 = do
      part <- await
      go (tags <> [part]) (n - 1)
    go tags n = do
      let fields = fromFoldable [ Tuple "mimetype" (show <<< unwrap $ getFileType state.file)
                                , Tuple "filename" state.name
                                , Tuple "size" (show <<< round $ size $ state.file)
                                , Tuple "parts" $ tags # joinWith ";"
                                ] <> getCommonFields state
      res <- lift $ attempt $ retry defaultRetryPolicy affjax $ defaultRequest
        { url = "https://upload.filestackapi.com/multipart/complete"
        , method = Left POST
        , content = Just (mkFormData fields)
        }
      case res of
        Left e -> lift $ liftEff $ throw $ show e
        Right r -> pure r.response

main :: Partial => Blob -> String -> Foreign -> Eff Effects Unit
main file name cfg = launchAff_ $ upload file init
  where init = { params: initialParams, file: file, name: name, cfg: cfg', partNum: 0 }
        cfg' = fromRight $ runExcept (read cfg)
