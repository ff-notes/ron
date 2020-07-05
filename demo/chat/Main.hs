import           RON.Store.FS (newHandle, runStore)
import           Text.Pretty.Simple (pPrint)

import           Database (loadAllMessages, newMessage)
import           Options (Command (Post, Show, UI), parseCommand)
import           UI (runUI)

main :: IO ()
main =
  do
    cmd <- parseCommand
    db <- newHandle "./data"
    case cmd of
      Show -> loadAllMessages db >>= pPrint
      Post username text -> do
        messageRef <- runStore db $ newMessage username text
        putStrLn $ "created message: " <> show messageRef
      UI uiOptions -> runUI db uiOptions
