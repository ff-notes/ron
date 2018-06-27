import           Control.Concurrent.Async (concurrently)
import           Test.Tasty.HUnit (assertEqual)

main :: IO ()
main = assertEqual "" ((), ()) =<< concurrently client server

client :: IO ()
client = pure ()

server :: IO ()
server = pure ()

-- В handshake
-- его нет в публичной спеке.
-- Но это фрейм запрос - фрейм ответ.
--
-- *db #name @connid :bookmark ?
--       :Password '123'
