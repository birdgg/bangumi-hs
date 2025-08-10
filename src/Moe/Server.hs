module Moe.Server (app) where

import Control.Monad.Except
import Moe.Api.Search
import Moe.Config (AppM, Config, runAppM)
import RIO hiding (Handler)
import Servant.API
import Servant.Server
import Servant.Util

type AppContext = '[]
type AppApi = "api" :> SearchApi

proxyContext :: Proxy AppContext
proxyContext = Proxy

proxyApi :: Proxy AppApi
proxyApi = Proxy

convertApp :: Config -> AppM Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runAppM cfg

configServer :: ServerT AppApi (AppM Config)
configServer = serverSearch

server :: Config -> Server AppApi
server cfg =
    hoistServerWithContext
        proxyApi
        proxyContext
        (convertApp cfg)
        configServer

app :: Context AppContext -> Config -> Application
app ctx = serveWithContext proxyApi (jsonErrorFormatters :. ctx) . server
