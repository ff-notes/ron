module UI (runUI) where

import           Brick (Widget, simpleMain, str)
import           RON.Store.FS (Handle)

runUI :: Handle -> IO ()
runUI _db = simpleMain ui

ui :: Widget ()
ui = str "Hello, world!"
