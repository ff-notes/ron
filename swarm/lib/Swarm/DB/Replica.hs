{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.DB.Replica (
    Replica (createReplica, open, receive),
    TextReplica,
    newTextReplica,
) where

import           Control.Exception (mask_)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy)
import           Foreign (FinalizerPtr, ForeignPtr, newForeignPtr)
import           Language.C.Inline.Context (ctxTypesTable)
import qualified Language.C.Inline.Cpp as Cpp
import           Language.C.Types (TypeSpecifier (TypeName))

import           RON.UUID (UUID (UUID))

import           Cxx.Std (stdCtx)
import qualified Cxx.Std.String as String
import           Swarm.RON.Status (Status (Status))
import qualified Swarm.RON.Status as Status
import           Swarm.RON.Text (TextFrame)

-- | Class @ron::Replica<TextFrame>@
newtype TextReplica = TextReplica (ForeignPtr (Proxy TextReplica))

Cpp.context
    $   Cpp.cppCtx
    <>  Cpp.bsCtx
    <>  Cpp.fptrCtx
    <>  stdCtx
    <>  mempty
        { ctxTypesTable = Map.fromList
            [ (TypeName "Status"     , [t| Proxy Status      |])
            , (TypeName "TextFrame"  , [t| Proxy TextFrame   |])
            , (TypeName "TextReplica", [t| Proxy TextReplica |])
            ]
        }
Cpp.include "<swarm/db/replica.hpp>"
Cpp.include "<swarm/ron/status.hpp>"
Cpp.include "<swarm/ron/text.hpp>"
Cpp.include "<swarm/ron/uuid.hpp>"
Cpp.verbatim
    "typedef ron::Replica<ron::RocksDBStore<ron::TextFrame>> TextReplica;"
Cpp.verbatim "typedef ron::Status Status;"
Cpp.verbatim "typedef ron::TextFrame TextFrame;"
Cpp.verbatim "typedef ron::Uuid Uuid;"
Cpp.verbatim "typedef std::string std_string;"

-- Cpp.verbatim "extern \"C\" void deleteTextFrame(TextFrame * p) { delete p; }"
-- foreign import ccall "&deleteTextFrame"
--     deleteTextFrame :: FinalizerPtr (Proxy TextFrame)

Cpp.verbatim
    "extern \"C\" void deleteTextReplica(TextReplica * p) { delete p; }"
foreign import ccall "&deleteTextReplica"
    deleteTextReplica :: FinalizerPtr (Proxy TextReplica)

-- | Template class @ron::Replica<>@
class Replica replica frame | frame -> replica, replica -> frame where

    -- | Method @Status Create(std::string home)@
    createReplica
        :: replica
        -> IO Status

    -- | Method @Status Create(std::string home)@
    open
        :: replica
        -> IO Status

    -- | Method @Status Receive(Builder& response, Cursor& query)@
    receive
        :: UUID  -- ^ object id
        -> UUID  -- ^ type
        -> replica
        -> IO (Either Status ByteString)

instance Replica TextReplica TextFrame where

    createReplica (TextReplica replica) =
        Status.decoding_ $ \statusPtr -> [Cpp.block| void {
            * $(Status * statusPtr) =
                $fptr-ptr:(TextReplica * replica)->CreateReplica();
        } |]

    open (TextReplica replica) =
        Status.decoding_ $ \statusPtr -> [Cpp.block| void {
            * $(Status * statusPtr) = $fptr-ptr:(TextReplica * replica)->Open();
        } |]

    receive (UUID objectIdX objectIdY)
            (UUID rdtX      rdtY     )
            (TextReplica replicaPtr) =
        Status.with $ \statusPtr ->
        String.with $ \resultDataPtr -> do
            [Cpp.block| void {
                Status & status = * $(Status * statusPtr);
                TextFrame::Builder result;
                TextFrame query = ron::Query<TextFrame>(
                    Uuid{$(uint64_t objectIdX), $(uint64_t objectIdY)},
                    Uuid{$(uint64_t rdtX     ), $(uint64_t rdtY     )}
                );
                TextFrame::Cursor qc{query};
                status =
                    $fptr-ptr:(TextReplica * replicaPtr)->Receive(result, qc);
                if (status)
                    * $(std_string * resultDataPtr) = result.data();
            } |]
            status <- Status.decode statusPtr
            case status of
                Status code _ | code == Status.ok ->
                    Right <$> String.decode resultDataPtr
                _ -> pure $ Left status

-- withTextFrame :: (Ptr (Proxy TextFrame) -> IO a) -> IO a
-- withTextFrame = bracket
--     [Cpp.exp| TextFrame * { new TextFrame } |]
--     (\p -> [Cpp.block| void { delete $(TextFrame * p); } |])

-- withTextReplica :: (TextReplica -> IO a) -> IO a
-- withTextReplica = bracket
--     (TextReplica <$> [Cpp.exp| TextReplica * { new TextReplica } |])
--     (\(TextReplica p) -> [Cpp.block| void { delete $(TextReplica * p); } |])

-- newForeignTextFrame :: IO (ForeignPtr (Proxy TextFrame))
-- newForeignTextFrame = mask_ $ do
--     p <- [Cpp.exp| TextFrame * { new TextFrame } |]
--     newForeignPtr deleteTextFrame p

newForeignTextReplica :: IO (ForeignPtr (Proxy TextReplica))
newForeignTextReplica = mask_ $ do
    p <- [Cpp.exp| TextReplica * { new TextReplica } |]
    newForeignPtr deleteTextReplica p

newTextReplica :: IO TextReplica
newTextReplica = TextReplica <$> newForeignTextReplica
