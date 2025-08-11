module Moe.Server (app) where

import Control.Monad.Except
import Moe.Api
import Moe.Api.Search
import Moe.Config (AppM, Config, runAppM)
import RIO hiding (Handler)
import Servant.API
import Servant.Server
import Servant.Util

type AppContext = '[]

proxyContext :: Proxy AppContext
proxyContext = Proxy

proxyApi :: Proxy Api
proxyApi = Proxy

convertApp :: Config -> AppM Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runAppM cfg

configServer :: ServerT Api (AppM Config)
configServer = serverSearch

server :: Config -> Server Api
server cfg =
    hoistServerWithContext
        proxyApi
        proxyContext
        (convertApp cfg)
        configServer

app :: Context AppContext -> Config -> Application
app ctx = serveWithContext proxyApi (jsonErrorFormatters :. ctx) . server
