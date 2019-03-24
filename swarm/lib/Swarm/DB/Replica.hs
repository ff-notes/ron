{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.DB.Replica (
    Replica (create, get),
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

import           Swarm.RON.Status (Status (Status))
import qualified Swarm.RON.Status as Status
import           Swarm.RON.Text (TextFrame (TextFrame))

-- | Class @ron::Replica<TextFrame>@
newtype TextReplica = TextReplica (ForeignPtr (Proxy TextReplica))

Cpp.context
    $   Cpp.cppCtx
    <>  Cpp.bsCtx
    <>  Cpp.fptrCtx
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
Cpp.verbatim "typedef ron::Status Status;"
Cpp.verbatim "typedef ron::TextFrame TextFrame;"
Cpp.verbatim "typedef ron::Replica<ron::TextFrame> TextReplica;"

-- | Template class @ron::Replica<>@
class Replica replica frame | frame -> replica, replica -> frame where

    -- | Method @Create(std::string home)@
    create
        :: ByteString  -- ^ path to db dir
        -> replica
        -> IO Status

    -- | Method @Get()@
    get :: UUID  -- ^ object id
        -> replica
        -> IO (Either Status frame)

instance Replica TextReplica TextFrame where

    create home (TextReplica replica) =
        Status.decoding
            [Cpp.exp| Status * {
                new(malloc(sizeof(Status)))
                Status($fptr-ptr:(TextReplica * replica)->Create($bs-cstr:home))
            } |]

    get (UUID x y) (TextReplica replica) = do
        frame <- newForeignTextFrame
        status <-
            Status.decoding
                [Cpp.exp| Status * {
                    new(malloc(sizeof(Status)))
                    Status(
                        $fptr-ptr:(TextReplica * replica)
                        ->Get(
                            * $fptr-ptr:(TextFrame * frame),
                            {$(uint64_t x), $(uint64_t y)}
                        )
                    )
                } |]
        pure $ case status of
            Status code _ | code == Status.ok -> Right $ TextFrame frame
            _                                 -> Left status

newForeignTextFrame :: IO (ForeignPtr (Proxy TextFrame))
newForeignTextFrame = mask_ $ do
    p <- [Cpp.exp| TextFrame * { new(malloc(sizeof(TextFrame))) TextFrame } |]
    newForeignPtr free p

newForeignTextReplica :: IO (ForeignPtr (Proxy TextReplica))
newForeignTextReplica = mask_ $ do
    p <- [Cpp.exp| TextReplica * {
        new(malloc(sizeof(TextReplica))) TextReplica
    } |]
    newForeignPtr free p

newTextReplica :: IO TextReplica
newTextReplica = TextReplica <$> newForeignTextReplica

foreign import ccall "&free" free :: FinalizerPtr a
