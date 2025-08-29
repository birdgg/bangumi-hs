module Moe.ThirdParty where

import BgmTV.Client
import Effectful
import Effectful.Reader.Static
import Mikan.Client
import Moe.Environment.Env
import Network.HTTP.Client qualified as Client
import Servant.Client

data RequestTarget = BgmTV | Mikan

requestThirdParty :: (IOE :> es, Reader MoeEnv :> es) => RequestTarget -> ClientM a -> Eff es (Either ClientError a)
requestThirdParty target req = do
  MoeEnv{httpManager} <- ask
  let clientEnv = mkClient target httpManager
  liftIO $ runClientM req clientEnv

mkClient :: RequestTarget -> Client.Manager -> ClientEnv
mkClient BgmTV httpManager = mkBgmClientEnv httpManager
mkClient Mikan httpManager = mkMikanClientEnv httpManager
