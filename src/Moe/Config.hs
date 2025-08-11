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
import RIO
import Servant.Client
import System.Environment
import Data.Pool hiding (createPool)
import Database.Persist.Sqlite hiding (LogFunc)

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

-- use for servant-auth
-- instance ThrowAll (AppM env a) where
--     throwAll = throwIO

liftAppM :: (MonadIO m, MonadReader env m) => AppM env a -> m a
liftAppM = liftRIO . unAppM

runAppM :: (MonadIO m) => env -> AppM env a -> m a
runAppM env = runRIO env . unAppM

data Config = Config
    { env :: Environment
    , port :: Int
    , logFunc :: (LogFunc, IO ())
    , pool :: ConnectionPool
    , bgmClientEnv :: ClientEnv
    , mikanClientEnv :: ClientEnv
    }

data Environment = Development | Production
    deriving stock (Eq, Read)

instance HasLogFunc Config where
    logFuncL = lens (fst . logFunc) const

instance HasBgmClientEnv Config where
    bgmClientEnvL = lens bgmClientEnv const

instance HasMikanClientEnv Config where
    mikanClientEnvL = lens mikanClientEnv const

class HasPool env where
   poolL :: Lens' env ConnectionPool

instance HasPool Config where
    poolL = lens pool const

createPool :: Environment -> LogFunc -> IO ConnectionPool
createPool env logFunc = runRIO logFunc $ do
    liftIO $ createSqlitePool dbPath 10
  where
    dbPath = case env of
        Development -> "./tmp/bangumi.db"
        Production -> "/app/data/bangumi.db"

lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting env def = do
    var <- lookupEnv env
    pure $ fromMaybe def $ var >>= readMaybe
