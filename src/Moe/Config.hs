module Moe.Config (
    AppM (..),
    Config (..),
    Environment (..),
    HasLogFunc (..),
    liftAppM,
    runAppM,
    lookupSetting,
) where

import BgmTV.Client
import Mikan.Client
import Network.HTTP.Client (Manager)
import RIO
import Servant.Client
import System.Environment

{- |
| App Monad
|
-}
newtype AppM env a = AppM {unAppM :: RIO env a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader env
        , MonadThrow
        , MonadUnliftIO
        , Semigroup
        )

liftAppM :: (MonadIO m, MonadReader env m) => AppM env a -> m a
liftAppM = liftRIO . unAppM

runAppM :: (MonadIO m) => env -> AppM env a -> m a
runAppM env = runRIO env . unAppM

{- |
| Application Environment
|
-}
data Config = Config
    { env :: Environment
    , port :: Int
    , logFunc :: (LogFunc, IO ())
    , httpManager :: Manager
    , bgmClientEnv :: BgmClientEnv
    , mikanClientEnv :: ClientEnv
    }

data Environment = Development | Production
    deriving stock (Eq, Read)

instance HasLogFunc Config where
    logFuncL = lens (fst . logFunc) const

instance HasBgmClientEnv Config where
    bgmClientEnvL = lens bgmClientEnv (\x y -> x{bgmClientEnv = y})

instance HasMikanClientEnv Config where
    mikanClientEnvL = lens mikanClientEnv (\x y -> x{mikanClientEnv = y})

{- |
| Utilities
|
-}
lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting env def = do
    var <- lookupEnv env
    pure $ fromMaybe def $ var >>= readMaybe
