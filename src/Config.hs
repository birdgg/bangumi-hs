module Config (
    AppM (..),
    App,
    Config (..),
    Environment (..),
    HasLogFunc (..),
    liftAppM,
    runAppM,
    createLogFunc,
    destroyLogFunc,
    lookupSetting,
) where

import Network.HTTP.Client (Manager)
import RIO
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

type App = AppM Config

{- |
| Application Environment
|
-}
data Config = Config
    { env :: Environment
    , logFunc :: (LogFunc, IO ())
    , httpManager :: Manager
    }

data Environment = Development | Production
    deriving stock (Eq, Read)

{- |
| Logging
|
-}
createLogFunc :: IO (LogFunc, IO ())
createLogFunc = do
    logOptions <- logOptionsHandle stdout True
    newLogFunc logOptions

destroyLogFunc :: (LogFunc, IO ()) -> IO ()
destroyLogFunc = snd

instance HasLogFunc Config where
    logFuncL = lens (fst . logFunc) const

{- |
| Utilities
|
-}
lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting env def = do
    var <- lookupEnv env
    pure $ fromMaybe def $ var >>= readMaybe
