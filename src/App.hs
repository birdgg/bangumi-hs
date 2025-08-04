module App (startApp) where

import Config
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import RIO
import Server (app)

startApp :: IO ()
startApp = do
    config <- setupConfig
    run 3000 (app config)

setupConfig :: IO Config
setupConfig = do
    manager <- Client.newManager tlsManagerSettings
    logFunc <- createLogFunc
    env <- lookupSetting "ENV" Development
    pure
        Config
            { env = env
            , logFunc = logFunc
            , httpManager = manager
            }
