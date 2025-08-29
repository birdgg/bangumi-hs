module Log.Backend.File where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Effectful
import Effectful.Log qualified as Log
import GHC.Generics (Generic)
import Log (Logger)
import Log.Internal.Logger (withLogger)
import System.IO (stdout)

newtype FileBackendConfig = FileBackendConfig
  { destinationFile :: FilePath
  }
  deriving stock (Eq, Generic, Ord, Show)

withJSONFileBackend ::
  forall (es :: [Effect]) (a :: Type).
  (IOE :> es) =>
  FileBackendConfig ->
  (Logger -> Eff es a) ->
  Eff es a
withJSONFileBackend FileBackendConfig{destinationFile} action = withRunInIO $ \unlift -> do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> destinationFile
  logger <- liftIO $ Log.mkLogger "file-json" $ \msg -> liftIO $ do
    let str = BSL.toStrict $ Aeson.encode msg <> "\n"
    BS.putStr str
    BS.appendFile destinationFile str
  withLogger logger (unlift . action)
