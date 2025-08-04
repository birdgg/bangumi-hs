module Server (app) where

import Config (App, AppM, Config, runAppM)
import Control.Monad.Except
import RIO hiding (Handler)
import RIO.Text qualified as T
import Servant.API
import Servant.Server

type Api = "api" :> Get '[PlainText] T.Text

api :: Proxy Api
api = Proxy

emptyHandler :: App T.Text
emptyHandler = pure "123"

convertApp :: Config -> AppM Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runAppM cfg

server :: Config -> Server Api
server cfg = hoistServer api (convertApp cfg) emptyHandler

app :: Config -> Application
app cfg = serve api (server cfg)
