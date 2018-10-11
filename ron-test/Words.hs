{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (words)

import           Control.Error (hoistEither)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (execStateT)
import           Data.List (intersect, sort, tails)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Metrics (levenshteinNorm)
import qualified Data.HashSet as HashSet

import           RON.Data (reduceObject')
import qualified RON.Data.RGA as RGA
import           RON.Event (applicationSpecific)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)

main :: IO ()
main = do
    words <- Text.lines <$> Text.getContents
    mapM_ print $ take 10 $ sort
        [   (   negate . realToFrac $ minimum
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
        , (branch1, branch2) <- distinctPairs words
        , let end = rgaTrick begin branch1 branch2
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

distinctPairs :: [a] -> [(a, a)]
distinctPairs xs = [(a, b) | a : xs' <- tails xs, b <- xs']

stems :: Text -> [Text]
stems word
    =   word
    :   [ s
        | suffix <- ["'s", "ed", "es", "s", "ing"]
        , Just s <- [Text.stripSuffix suffix word]
        ]
    ++  [ s <> "e"
        | suffix <- ["ed", "es"], Just s <- [Text.stripSuffix suffix word]
        ]

rgaTrick :: Text -> Text -> Text -> Text
rgaTrick begin branch1 branch2 =
    either error id .
    runNetworkSim . runReplicaSim (applicationSpecific 58) . runExceptT $ do
        begin' <- RGA.fromList $ Text.unpack begin
        branch1' <- (`execStateT` begin') . RGA.edit $ Text.unpack branch1
        branch2' <- (`execStateT` begin') . RGA.edit $ Text.unpack branch2
        end' <- hoistEither $ reduceObject' branch1' branch2'
        hoistEither $ Text.pack <$> RGA.toList end'
