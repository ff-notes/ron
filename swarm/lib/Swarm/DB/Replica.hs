{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.DB.Replica (
    Replica (create, open, get, receiveQuery),
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
import           Swarm.RON.Text (TextFrame (TextFrame))

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
Cpp.verbatim "typedef ron::Replica<ron::TextFrame> TextReplica;"
Cpp.verbatim "typedef ron::Status Status;"
Cpp.verbatim "typedef ron::TextFrame TextFrame;"
Cpp.verbatim "typedef std::string std_string;"

Cpp.verbatim "extern \"C\" void deleteTextFrame(TextFrame * p) { delete p; }"
foreign import ccall "&deleteTextFrame"
    deleteTextFrame :: FinalizerPtr (Proxy TextFrame)

Cpp.verbatim "extern \"C\" void deleteTextReplica(TextReplica * p) {delete p;}"
foreign import ccall "&deleteTextReplica"
    deleteTextReplica :: FinalizerPtr (Proxy TextReplica)

-- | Template class @ron::Replica<>@
class Replica replica frame | frame -> replica, replica -> frame where

    -- | Method @Status Create(std::string home)@
    create
        :: ByteString  -- ^ path to db dir
        -> replica
        -> IO Status

    -- | Method @Status Create(std::string home)@
    open
        :: ByteString  -- ^ path to db dir
        -> replica
        -> IO Status

    -- | Method @Status Get(Frame& object, Uuid id)@
    get :: UUID  -- ^ object id
        -> replica
        -> IO (Either Status frame)

    -- | Method
    -- @Status
    -- ReceiveQuery(Builder& response, Uuid object_store, Cursor& query)@
    receiveQuery
        :: UUID        -- ^ object store
        -> ByteString  -- ^ query
        -> replica
        -> IO (Either Status ByteString)

instance Replica TextReplica TextFrame where

    create home (TextReplica replica) =
        Status.decoding_ $ \statusPtr -> [Cpp.block| void {
            * $(Status * statusPtr) =
                $fptr-ptr:(TextReplica * replica)->Create($bs-cstr:home);
        } |]

    open home (TextReplica replica) =
        Status.decoding_ $ \statusPtr -> [Cpp.block| void {
            * $(Status * statusPtr) =
                $fptr-ptr:(TextReplica * replica)->Open($bs-cstr:home);
        } |]

    get (UUID x y) (TextReplica replica) = do
        framePtr <- newForeignTextFrame
        status <- Status.decoding_ $ \statusPtr -> [Cpp.block| void {
            * $(Status * statusPtr) =
                $fptr-ptr:(TextReplica * replica)
                ->Get(
                    * $fptr-ptr:(TextFrame * framePtr),
                    {$(uint64_t x), $(uint64_t y)}
                );
        } |]
        pure $ case status of
            Status code _ | code == Status.ok -> Right $ TextFrame framePtr
            _                                 -> Left status

    receiveQuery _objectStore query (TextReplica replicaPtr) =
        Status.with $ \statusPtr ->
        String.with $ \resultDataPtr -> do
            [Cpp.block| void {
                Status & status = * $(Status * statusPtr);
                std::string termd{$bs-cstr:query};
                if (termd.empty()) {
                    status = Status::BADARGS;
                    return;
                }
                ron::Uuid id{}, rdt{};
                size_t dot;
                if (termd.find('?') == -1)
                    termd.push_back('?');
                TextFrame::Cursor cur{termd};
                if (!cur.valid()) {
                    status = Status::BADARGS.comment("need a query op");
                    return;
                }
                id = cur.id();
                rdt = cur.ref();
                if (id == ron::Uuid::FATAL || rdt == ron::Uuid::FATAL) {
                    status = Status::BADARGS.comment("not an UUID");
                    return;
                }
                TextFrame::Builder result;
                status =
                    $fptr-ptr:(TextReplica * replicaPtr)
                    ->ReceiveQuery(result, ron::Uuid::NIL, cur);
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

newForeignTextFrame :: IO (ForeignPtr (Proxy TextFrame))
newForeignTextFrame = mask_ $ do
    p <- [Cpp.exp| TextFrame * { new TextFrame } |]
    newForeignPtr deleteTextFrame p

newForeignTextReplica :: IO (ForeignPtr (Proxy TextReplica))
newForeignTextReplica = mask_ $ do
    p <- [Cpp.exp| TextReplica * { new TextReplica } |]
    newForeignPtr deleteTextReplica p

newTextReplica :: IO TextReplica
newTextReplica = TextReplica <$> newForeignTextReplica
