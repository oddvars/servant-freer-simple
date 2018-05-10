{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Category
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Control.Monad.Freer
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant

import Prelude hiding ((.))

-- Capitalize effect
data Capitalize r where
  Upper :: Maybe T.Text -> Capitalize T.Text
  Lower :: Maybe T.Text -> Capitalize T.Text

upper :: Member Capitalize r => Maybe T.Text -> Eff r T.Text
upper = send . Upper

lower :: Member Capitalize r => Maybe T.Text -> Eff r T.Text
lower = send . Lower

runCapitalize :: Eff (Capitalize ': r) w -> Eff r w
runCapitalize = interpret $ \case
  (Upper s) -> pure (T.toUpper (fromMaybe "" s))
  (Lower s) -> pure (T.toLower (fromMaybe "" s))

-- effects wrapper
type Effect = Eff '[Capitalize, IO]

runEffect :: Effect a -> IO a
runEffect = runM . runCapitalize

-- servant api
type API =
  "upper" :> QueryParam "s" T.Text :> Get '[JSON] T.Text :<|>
  "lower" :> QueryParam "s" T.Text :> Get '[JSON] T.Text

api :: Proxy API
api = Proxy

effToHandler :: Effect :~> Handler
effToHandler = NT (liftIO . runEffect)

serverT :: ServerT API Effect
serverT = upper
     :<|> lower

server :: Server API
server = enter effToHandler serverT

app :: Application
app = serve api server

main :: IO ()
main = W.run 8088 app
