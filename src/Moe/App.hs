module Moe.App where

import BgmTV.Client
import Mikan.Client
import Moe.Config
import Moe.Log
import Moe.Middlewares
import Moe.Server
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import RIO
import Servant

main :: IO ()
main = bracket acquireConfig shutdownApp startApp

startApp :: Config -> IO ()
startApp env = do
    let
        context :: Context '[]
        context = EmptyContext

        port :: Int
        port = env.port

    run port $
        addMiddlewares env $
            app context env

shutdownApp :: Config -> IO ()
shutdownApp config = do
    destroyLogFunc config.logFunc

acquireConfig :: IO Config
acquireConfig = do
    port <- lookupSetting "PORT" 3000
    env <- lookupSetting "ENV" Development
    logFunc <- createLogFunc
    httpManager <- newManager tlsManagerSettings
    bgm <- mkBgmClientEnv httpManager
    mikan <- mkMikanClientEnv httpManager
    pure
        Config
            { logFunc = logFunc
            , env = env
            , port = port
            , httpManager = httpManager
            , bgmClientEnv = bgm
            , mikanClientEnv = mikan
            }
