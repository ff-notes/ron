{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cxx.Std.String (
    String,
    decode,
    with,
) where

import           Prelude hiding (String)

import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Proxy (Proxy)
import           Foreign (Ptr)
import qualified Language.C.Inline.Cpp as Cpp

import           Cxx.Std (String, stdCtx)

Cpp.context $ Cpp.cppCtx <> stdCtx
Cpp.include "<string>"
Cpp.verbatim "typedef std::string std_string;"

with :: (Ptr (Proxy String) -> IO a) -> IO a
with = bracket
    [Cpp.exp| std_string * { new std::string } |]
    (\p -> [Cpp.block| void { delete $(std_string * p); } |])

decode :: Ptr (Proxy String) -> IO ByteString
decode ptr = do
    (dat, len) <-
        (,) <$> [Cpp.exp| char const * { $(std_string * ptr)->data()   } |]
            <*> [Cpp.exp| size_t       { $(std_string * ptr)->length() } |]
    BS.packCStringLen (dat, fromIntegral len)
