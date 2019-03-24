{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cxx.Std.String (
    String,
    decode,
    newForeign,
) where

import           Prelude hiding (String)

import           Control.Exception (mask_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Proxy (Proxy)
import           Foreign (FinalizerPtr, ForeignPtr, newForeignPtr,
                          withForeignPtr)
import qualified Language.C.Inline.Cpp as Cpp

import           Cxx.Std (String, stdCtx)

Cpp.context $ Cpp.cppCtx <> stdCtx
Cpp.include "<string>"
Cpp.verbatim "typedef std::string std_string;"

Cpp.verbatim "extern \"C\" void deleteStdString(std::string * p) {delete p;}"
foreign import ccall "&deleteStdString"
    deleteStdString :: FinalizerPtr (Proxy String)

newForeign :: IO (ForeignPtr (Proxy String))
newForeign = mask_ $ do
    p <- [Cpp.exp| std_string * { new std::string } |]
    newForeignPtr deleteStdString p

decode :: ForeignPtr (Proxy String) -> IO ByteString
decode fptr = do
    (ptr, len) <- withForeignPtr fptr $ \ptr ->
        (,) <$> [Cpp.exp| char const * { $(std_string * ptr)->data()   } |]
            <*> [Cpp.exp| size_t       { $(std_string * ptr)->length() } |]
    BS.packCStringLen (ptr, fromIntegral len)
