{-# LANGUAGE OverloadedStrings #-}

module HexDump where

import           Internal.Prelude
import           Numeric (showHex)

import qualified Data.ByteString.Lazy as BSL

hexdump :: ByteStringL -> String
hexdump = unlines . map showLine . split
  where
    showLine s =
        show8 s1 ++ "  " ++ show8 s2 ++ " | " ++
        init (tail $ show $ BSL.map (sub 0 32) s)
      where
        (s1, s2) = BSL.splitAt 8 s
    show8 s = unwords (map showHex2 bs) ++ replicate (24 - 3 * length bs) ' '
      where
        bs = BSL.unpack s
    showHex2 b
        | b < 16    = '0' : showHex b ""
        | otherwise = showHex b ""
    split s = case BSL.splitAt 16 s of
        ("",    _   ) -> []
        (chunk, ""  ) -> [chunk]
        (chunk, rest) -> chunk : split rest
    sub x y z = if x == z then y else z
