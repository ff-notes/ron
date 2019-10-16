{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.RON.Status
  ( Status (..),
    decode,
    decoding,
    decoding_,
    with,

    -- * Statuses
    ok,
    noType,
    notFound,
    notOpen,
  )
where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy)
import Foreign (Ptr)
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Inline.Cpp (withPtrs_)
import qualified Language.C.Inline.Cpp as Cpp
import Language.C.Types (TypeSpecifier (TypeName))
import RON.UUID (UUID (UUID))

-- | Class @ron::Status@
data Status = Status {code :: UUID, comment :: ByteString}
  deriving (Eq, Show)

Cpp.context $
  Cpp.cppCtx
    <> mempty
      { ctxTypesTable = Map.singleton (TypeName "Status") [t|Proxy Status|]
      }

Cpp.include "<swarm/ron/status.hpp>"

Cpp.verbatim "typedef ron::Status Status;"

ok :: UUID
ok =
  UUID
    [Cpp.pure| uint64_t { uint64_t(Status::OK.code().value ()) } |]
    [Cpp.pure| uint64_t { uint64_t(Status::OK.code().origin()) } |]

--     | ENDOFFRAME
--     | NOT_IMPLEMENTED

notFound :: UUID
notFound =
  UUID
    [Cpp.pure| uint64_t { uint64_t(Status::NOT_FOUND.code().value ()) } |]
    [Cpp.pure| uint64_t { uint64_t(Status::NOT_FOUND.code().origin()) } |]

--     | BAD_STATE
--     | BADARGS
--     | BADSYNTAX
--     | DB_FAIL
--     | IOFAIL
--     | BADFRAME
--     | BADID
--     | BADREF
--     | BADVALUE

noType :: UUID
noType =
  UUID
    [Cpp.pure| uint64_t { uint64_t(Status::NOTYPE.code().value ()) } |]
    [Cpp.pure| uint64_t { uint64_t(Status::NOTYPE.code().origin()) } |]

notOpen :: UUID
notOpen =
  UUID
    [Cpp.pure| uint64_t { uint64_t(Status::NOTOPEN.code().value ()) } |]
    [Cpp.pure| uint64_t { uint64_t(Status::NOTOPEN.code().origin()) } |]

--     | CHAINBREAK
--     | HASHBREAK
--     | TREEBREAK
--     | CAUSEBREAK

--     | TREEGAP
--     | YARNGAP

--     | REPEAT
--     | REORDER

decode :: Ptr (Proxy Status) -> IO Status
decode statusPtr = do
  (x, y, ptr, len) <- withPtrs_ $ \(x, y, ptr, len) ->
    [Cpp.block| void {
      Status & status = * $(Status * statusPtr);
      * $(uint64_t * x)      = uint64_t(status.code().value());
      * $(uint64_t * y)      = uint64_t(status.code().origin());
      * $(char const ** ptr) = status.comment().data();
      * $(size_t * len)      = status.comment().length();
    } |]
  comment <- BS.packCStringLen (ptr, fromIntegral len)
  pure Status {code = UUID x y, comment}

with :: (Ptr (Proxy Status) -> IO a) -> IO a
with =
  bracket
    [Cpp.exp| Status * { new Status } |]
    (\p -> [Cpp.block| void { delete $(Status * p); } |])

decoding :: (Ptr (Proxy Status) -> IO a) -> IO (Status, a)
decoding action =
  with $ \ptr -> do
    a <- action ptr
    status <- decode ptr
    pure (status, a)

decoding_ :: (Ptr (Proxy Status) -> IO ()) -> IO Status
decoding_ = fmap fst . decoding
