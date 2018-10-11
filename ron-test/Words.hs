{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (words)

import           Control.Error (hoistEither)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (execStateT)
import qualified Data.HashSet as HashSet
import           Data.List (intersect, sortOn, tails)
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Metrics (levenshteinNorm)

import           RON.Data (reduceObject')
import           RON.Data.RGA (RgaString)
import qualified RON.Data.RGA as RGA
import           RON.Event (applicationSpecific)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Types (Object)

main :: IO ()
main = do
    words <- Text.lines <$> Text.getContents
    mapM_ print $ take 10 $ sortOn Down
        [   (   realToFrac $ minimum
                    [ levenshteinNorm begin branch1
                    , levenshteinNorm begin branch2
                    , levenshteinNorm branch1 end
                    , levenshteinNorm branch2 end
                    ]
                :: Double
            , begin
            , (branch1, branch2)
            , end
            )
        | begin <- words
        , branch1 : words' <- tails words
        , let (begin', branch1') = rgaTrick1 begin branch1
        , branch2 <- words'
        , let end = rgaTrick2 begin' branch1' branch2
        , end `HashSet.member` HashSet.fromList words
        , and
            [ null $ a `intersect` b
            | (a, b) <-
                [ (stems begin, stems branch1)
                , (stems begin, stems branch2)
                , (stems end,   stems branch1)
                , (stems end,   stems branch2)
                ]
            ]
        ]

stems :: Text -> [Text]
stems word
    =   word
    :   [ s
        | suffix <- ["'s", "ed", "es", "s", "ing", "ian"]
        , Just s <- [Text.stripSuffix suffix word]
        ]
    ++  [ s <> "e"
        | suffix <- ["ed", "es"], Just s <- [Text.stripSuffix suffix word]
        ]

rgaTrick1 :: Text -> Text -> (Object RgaString, Object RgaString)
rgaTrick1 begin branch1 =
    either error id .
    runNetworkSim . runReplicaSim (applicationSpecific 1) . runExceptT $ do
        begin' <- RGA.fromList $ Text.unpack begin
        branch1' <- (`execStateT` begin') . RGA.edit $ Text.unpack branch1
        pure (begin', branch1')

rgaTrick2 :: Object RgaString -> Object RgaString -> Text -> Text
rgaTrick2 begin' branch1' branch2 =
    either error id .
    runNetworkSim . runReplicaSim (applicationSpecific 2) . runExceptT $ do
        branch2' <- (`execStateT` begin') . RGA.edit $ Text.unpack branch2
        end' <- hoistEither $ reduceObject' branch1' branch2'
        hoistEither $ Text.pack <$> RGA.toList end'
