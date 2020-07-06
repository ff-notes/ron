import           RON.Store.FS (newHandle, runStore)
import           Text.Pretty.Simple (pPrint)

import           Database (loadAllMessages, newMessage)
import           Options (Command (Post, Show, UI), parseCommand)
import           Types (MessageContent (MessageContent))
import qualified Types
import           UI (runUI)

main :: IO ()
main = do
  cmd <- parseCommand
  db <- newHandle "./data"
  case cmd of
    Show -> loadAllMessages db >>= pPrint
    Post username text -> do
      messageRef <- runStore db $ newMessage MessageContent{username, text}
      putStrLn $ "created message: " <> show messageRef
    UI uiOptions -> runUI db uiOptions
