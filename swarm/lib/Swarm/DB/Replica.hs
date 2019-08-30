{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.DB.Replica
  ( TextReplica,
    createReplica,
    newTextReplica,
    open,
    receive
    )
where

import Control.Exception (mask_)
import Cxx.Std (stdCtx)
import qualified Cxx.Std.String as String
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy)
import Foreign (FinalizerPtr, ForeignPtr, newForeignPtr)
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Inline.Cpp (context)
import qualified Language.C.Inline.Cpp as Cpp
import Language.C.Types (TypeSpecifier (TypeName))
import RON.UUID (UUID (UUID))
import Swarm.RON.Status (Status (Status))
import qualified Swarm.RON.Status as Status
import Swarm.RON.Text (TextFrame)

-- | Class @ron::Replica<TextFrame>@
newtype TextReplica = TextReplica (ForeignPtr (Proxy TextReplica))

do
  context
    $ cppCtx
    <> bsCtx
    <> fptrCtx
    <> stdCtx
    <> mempty
      { ctxTypesTable =
          Map.fromList
            [ (TypeName "Status",      [t|Proxy Status|]),
              (TypeName "TextFrame",   [t|Proxy TextFrame|]),
              (TypeName "TextReplica", [t|Proxy TextReplica|])
              ]
        }
  include "<swarm/db/replica.hpp>"
  include "<swarm/ron/status.hpp>"
  include "<swarm/ron/text.hpp>"
  include "<swarm/ron/uuid.hpp>"
  verbatim
    "typedef ron::Replica<ron::RocksDBStore<ron::TextFrame>> TextReplica;"
  verbatim "typedef ron::Status Status;"
  verbatim "typedef ron::TextFrame TextFrame;"
  verbatim "typedef ron::Uuid Uuid;"
  verbatim "typedef std::string std_string;"
  -- verbatim "extern \"C\" void deleteTextFrame(TextFrame * p) { delete p; }"
  verbatim "extern \"C\" void deleteTextReplica(TextReplica * p) { delete p; }"

-- foreign import ccall "&deleteTextFrame"
--     deleteTextFrame :: FinalizerPtr (Proxy TextFrame)
--
foreign import ccall "&deleteTextReplica"
  deleteTextReplica :: FinalizerPtr (Proxy TextReplica)

-- | Method @Status CreateReplica()@
createReplica :: TextReplica -> IO Status
createReplica (TextReplica replica) =
  Status.decoding_ $ \statusPtr ->
    [block| void {
      * $(Status * statusPtr) =
        $fptr-ptr:(TextReplica * replica)->CreateReplica();
      } |]

-- | Method @Status Open()@
open :: TextReplica -> IO Status
open (TextReplica replica) =
  Status.decoding_ $ \statusPtr ->
    [block| void {
      * $(Status * statusPtr) = $fptr-ptr:(TextReplica * replica)->Open();
      } |]

-- | Method @Status Receive(Builder& response, Cursor& query)@
receive
  :: UUID -- ^ object id
  -> UUID -- ^ type
  -> TextReplica
  -> IO (Either Status ByteString)
receive (UUID objectIdX objectIdY) (UUID rdtX rdtY) (TextReplica replicaPtr) =
  Status.with $ \statusPtr ->
    String.with $ \resultDataPtr -> do
      [block| void {
        Status & status = * $(Status * statusPtr);
        TextFrame::Builder result;
        TextFrame query = ron::Query<TextFrame>(
          Uuid{$(uint64_t objectIdX), $(uint64_t objectIdY)},
          Uuid{$(uint64_t rdtX     ), $(uint64_t rdtY     )}
          );
        TextFrame::Cursor qc{query};
        status = $fptr-ptr:(TextReplica * replicaPtr)->Receive(result, qc);
        if (status)
          * $(std_string * resultDataPtr) = result.data();
        } |]
      status <- Status.decode statusPtr
      case status of
        Status code _
          | code == Status.ok -> Right <$> String.decode resultDataPtr
        _ -> pure $ Left status

-- withTextFrame :: (Ptr (Proxy TextFrame) -> IO a) -> IO a
-- withTextFrame = bracket
--     [exp| TextFrame * { new TextFrame } |]
--     (\p -> [block| void { delete $(TextFrame * p); } |])

-- withTextReplica :: (TextReplica -> IO a) -> IO a
-- withTextReplica = bracket
--     (TextReplica <$> [exp| TextReplica * { new TextReplica } |])
--     (\(TextReplica p) -> [block| void { delete $(TextReplica * p); } |])

-- newForeignTextFrame :: IO (ForeignPtr (Proxy TextFrame))
-- newForeignTextFrame = mask_ $ do
--     p <- [exp| TextFrame * { new TextFrame } |]
--     newForeignPtr deleteTextFrame p
--
newForeignTextReplica :: IO (ForeignPtr (Proxy TextReplica))
newForeignTextReplica = mask_ $ do
  p <- [exp| TextReplica * { new TextReplica } |]
  newForeignPtr deleteTextReplica p

newTextReplica :: IO TextReplica
newTextReplica = TextReplica <$> newForeignTextReplica
