import Control.Monad (when)
import Control.Monad.Logger (MonadLogger, runFileLoggingT)
import Data.Text (Text)
import RON.Store.Sqlite (runStore)
import RON.Store.Sqlite qualified as Store
import RON.Types.Experimental (Ref (Ref))
import Text.Pretty.Simple (pPrint)
import UnliftIO (MonadUnliftIO, liftIO, newTChanIO)

import Database qualified
import Fork (forkLinked)
import NetNode qualified
import Options (
    Command (Edit, Post, RunNode, RunUI, Show),
    NodeOptions (NodeOptions),
    Options (Options),
    UIOptions (UIOptions),
    parseOptions,
 )
import Options qualified
import Types (Env (Env), Message (Message))
import Types qualified
import UI (initUI, runUI)

main :: IO ()
main = do
    Options{database, cmd, logFile} <- parseOptions
    runFileLoggingT logFile do
        db <- Store.newHandle database
        case cmd of
            Show -> Database.loadAllMessages db >>= pPrint
            Post{username, text} -> do
                messageRef <-
                    runStore db $ Database.newMessage Message{username, text}
                liftIO $ putStrLn $ "created message: " <> show messageRef
            Edit{messageId, text} -> do
                runStore db $
                    Database.editMessageText (Ref @Message messageId) text
                liftIO $ putStrLn "edited message"
            RunNode nodeOptions -> runNode db nodeOptions
            RunUI UIOptions{username} nodeOptions -> do
                forkLinked $ runNode db nodeOptions
                runUI' username db

runUI' :: (MonadLogger m, MonadUnliftIO m) => Text -> Store.Handle -> m ()
runUI' username db = do
    onMessagePosted <- newTChanIO
    onMessageListUpdated <- newTChanIO
    let env = Env{username, onMessagePosted, onMessageListUpdated}
    uiHandle <- initUI db env
    forkLinked $ Database.databaseToUIUpdater db onMessageListUpdated
    forkLinked $ Database.messagePoster onMessagePosted db
    runUI uiHandle

runNode ::
    (MonadFail m, MonadLogger m, MonadUnliftIO m) =>
    Store.Handle -> NodeOptions -> m ()
runNode db options@NodeOptions{listenPorts, peers} = do
    when (null listenPorts && null peers) $
        fail
            "The peer must connect to other peers or listen for connections. \
            \Specify `--listen` or `--peer`."
    NetNode.workers db options
