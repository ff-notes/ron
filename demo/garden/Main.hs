import           Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import           Data.Tree (Tree (Node))
import           Text.Pretty.Simple (pPrint)
import           UnliftIO (MonadUnliftIO)

import           RON.Data.GTree (GTree)
import qualified RON.Data.GTree as GTree
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store (Handle, newHandle)
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID

import           Options

main :: IO ()
main = do
  Options{database, cmd} <- parseOptions
  runStderrLoggingT do
    db <- Store.newHandle database
    case cmd of
      Show -> loadTheTree db >>= pPrint
      Post{} -> undefined
      --   messageRef <-
      --     runStore db $ Database.newMessage MessageContent{username, text}
      --   liftIO $ putStrLn $ "created message: " <> show messageRef
      RunNode _nodeOptions -> undefined
        -- runNode db nodeOptions
      RunUI UIOptions{} _nodeOptions -> undefined
      --   forkLinked $ runNode db nodeOptions
      --   runUI' username db

loadTheTree :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m (Tree UUID)
loadTheTree db =
  runStore db do
    forest <- GTree.loadForest theTreeRef
    pure $ Node theTreeId $ map (fmap opId) forest

theTreeId :: UUID
theTreeId = $(UUID.liftName "theTree")

theTreeRef :: Ref GTree
theTreeRef = Ref theTreeId []
