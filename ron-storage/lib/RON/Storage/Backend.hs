{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | RON Storage details. Use of this module only to implement a backend.
module RON.Storage.Backend
  ( Collection (..),
    CollectionName,
    ObjectRef (..),
    Document (..),
    ObjectVersion,
    IsTouched (..),
    MonadStorage (..),
    RawDocId,
    createVersion,
    decodeDocId,
    readVersion,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String (fromString)
import           System.FilePath ((</>))
import qualified Text.Show (show)

import           RON.Data (Replicated)
import           RON.Error (MonadE, liftMaybe, throwErrorString)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Prelude
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (DocFrame (DocFrame, frame, uuid), UUID)
import           RON.Util (ByteStringL)
import qualified RON.UUID as UUID

-- | Storage backend interface
class (ReplicaClock m, MonadE m) => MonadStorage m where
  type CollectionName m

  getCollections :: m [CollectionName m]

  -- | Must return @[]@ for non-existent collection
  getDocuments :: CollectionName m -> m [ObjectRef a]

  -- | Must return @[]@ for non-existent object
  getDocumentVersions :: CollectionName m -> ObjectRef a -> m [ObjectVersion]

  -- | Must create collection and object if not exist
  saveVersionContent
    :: Collection a => ObjectRef a -> ObjectVersion -> ByteStringL -> m ()

  loadVersionContent :: Collection a => ObjectRef a -> ObjectVersion -> m ByteStringL

  deleteVersion :: Collection a => ObjectRef a -> ObjectVersion -> m ()

  changeDocId :: Collection a => ObjectRef a -> ObjectRef a -> m ()

-- | Try decode UUID from a file name
decodeDocId
  :: ObjectRef a
  -> Maybe (Bool, UUID) -- ^ Bool = is object id a valid UUID encoding
decodeDocId (ObjectRef file) = do
  uuid <- UUID.decodeBase32 file
  pure (UUID.encodeBase32 uuid == file, uuid)

-- | Load object version as an object
readVersion ::
    (Collection a, MonadStorage m) =>
    ObjectRef a -> ObjectVersion -> m (DocFrame a, IsTouched)
readVersion docid version = do
  (isObjectIdValid, uuid) <-
    liftMaybe ("Bad Base32 UUID " <> show docid)
      $ decodeDocId docid
  unless isObjectIdValid
    $ throwErrorString
    $ "Not a Base32 UUID "
      ++ show docid
  contents <- loadVersionContent docid version
  case parseStateFrame contents of
    Right frame -> pure (DocFrame{uuid, frame}, IsTouched False)
    Left ronError -> do
        object <- fallbackParse uuid contents
        pure (object, IsTouched True)
        `catchError` \fallbackError ->
          throwError $ case BSLC.head contents of
            '{' -> fallbackError
            _ -> fromString ronError

-- | Indicator of fact that thing (e.g. object) was fixed during loading.
-- It it was fixed during loading it must be saved to the storage.
newtype IsTouched = IsTouched Bool
  deriving (Show)

-- | Result of DB reading, loaded object with information about its versions
data Document a = Document {
    docFrame  :: DocFrame a,
    versions  :: NonEmpty ObjectVersion,
    isTouched :: IsTouched
    }
    deriving (Show)

-- | Create new version of an object/object and delete older versions.
-- If the object doesn't exist yet, it will be created.
createVersion ::
    forall a m.
    (Collection a, MonadStorage m) =>
    -- | 'Just', if object exists already; 'Nothing' otherwise.
    Maybe (ObjectRef a, Document a) ->
    DocFrame a ->
    m ()
createVersion mDoc newFrame = case mDoc of
    Nothing -> save (ObjectRef @a $ UUID.encodeBase32 uuid) []
    Just (docid, oldDoc) -> do
        let Document {
                docFrame = oldFrame,
                versions,
                isTouched = IsTouched isTouched
                } =
                    oldDoc
        when (newFrame /= oldFrame || length versions /= 1 || isTouched) $
            save docid $ toList versions
    where
        DocFrame{uuid, frame} = newFrame
        save docid oldVersions = do
            newVersion <- UUID.encodeBase32 <$> getEventUuid
            saveVersionContent docid newVersion (serializeStateFrame frame)
            for_ oldVersions $ deleteVersion docid
