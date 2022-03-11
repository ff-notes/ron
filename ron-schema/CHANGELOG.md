# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [0.10]
### Added
- Positions to syntax errors.
- Generate `Editable` instances for struct_set

### Changed
- Struct field syntax made explicit.
- `struct_set` field merge strategy is now explicit positional attribute,
  not an annotation.
- New field syntax:
  ```
  (struct_lww STRUCT_NAME (FIELD_NAME TYPE))
  (struct_set STRUCT_NAME (FIELD_NAME MERGE_STRATEGY TYPE))
  ```

## [0.9] - 2019-09-06
### Added
- Generate `struct_set.$(field)_zoom` for opaque object field
- Generate `struct_set.$(field)_get` for object field
- `ObjectRef` is available in the schema language
- Generate `struct_set.$(field)_add`
- Generate `struct_set.$(field)_remove` for set-merged fields
- Generate `struct_set.$(field)_removeIf` for set-merged fields

### Changed
- `opaque atoms`  is now `opaque_atoms`
- `opaque object` is now `opaque_object`
- `struct_set.$(field)_assign` is split into:
  - `struct_set.$(field)_set` now accepts only "whole" (not Nothing) values
  - `struct_set.$(field)_clear` assigns Nothing
- Renamed `struct_lww.$(field)_assign` to `struct_lww.$(field)_set`
- Type of `struct_set` fields with merge strategy `set` is now `[]` and its
  value contains all alive versions.

### Removed
- `TComposite`, now `TEnum _ :: RonType`

## [0.8] - 2019-08-10
### Added
- `Float` atom type
- `UUID` atom type
- Struct field annotations
- `struct_set` to define an ORSet-based structure

### Changed
- LWW
  - fields are always optional
  - `viewField` returns `Maybe`
  - `readField` returns `Maybe`
  - `assignField` takes `Maybe`

### Removed
- `Option` type

## [0.7] - 2019-07-26
### Added
- `alias` declaration.
- `ORSet.Map` type.

### Changed
- Renamed `Boole` -> `Bool` due to tradition.
- `ORSet` don't generate `ObjectORSet` wrapper since `ORSet` now manages
  objects too.
- Now `ObjectState` keeps a typed reference to an object with state frame
  attached,
  and `Object` is just a type UUID --
  a typed reference to an object in a state frame passed in
  `MonadObjectState` context.
  Object is now passed as an explicit argument.

### Removed
- Concept of view type. Maybe will return later.

## [0.6] - 2019-04-25
### Added
- Schema language: `RGA` type.

## [0.5] - 2019-02-04
### Added
- `RON.UUID.liftName` function to create name UUIDs in compile time.
- `RON.Util.ByteStringL` type.
- `RON.Error` module with unified pretty errors.
- Organize `Replicated`, `ReplicatedAsPayload`, and `ReplicatedAsObject` in
  class hierarchy.
- Add `ORSet.removeValue` and `removeRef` implementation.
- Op "patterns" and patterns.

### Removed
- Type alias `ObjectId` since objects are identified by UUID.

### Changed
- Extracted `ron-storage` package.
- Extracted `ron-schema` package.
- Extracted `ron-rdt` package.
- Switched from `Either String a` to `MonadError String m => m a` in failable
  procedures.
- `ORSet.addRef` now adds item's frame, too.
- `ORSet.addNewRef` now returns the reference to the freshly created object.
- Change `StateFrame` key to UUID since objects are identified by UUID.
- Renamed `RawOp` to `ClosedOp` according to the fresh spec.

### Fixed
- Error handling in Boole decoder.

## [0.4] - 2019-01-09
### Added
- Schema `enum` declaration.
- `docIdFromUuid`.
- `OnDocumentChanged` is called each time when any document is changed.

### Changed
- Made GHC 8.6 default.

### Removed
- Schema embedded DSL helpers: `atomInteger`, `atomString`, `boole`, `char`,
  `field`, `option`, `orSet`, `rgaString`, `structLww`, `versionVector`.

### Fixed
- `RGA.edit` bug with re-adding deleted items (#39).

## [0.3] - 2018-12-05
### Added
- Encode/decode EpochTime.
- EDN-based schema DSL.

### Removed
- `RON.Storage.createVersion` from public API.
- `NFData` instances.

## [0.2] - 2018-11-20
### Added
- Schema boole type.
- RON.Storage and submodules are moved from ff project.
- RON.Schema is now re-exported via RON.Schema.TH.

### Changed
- Renamed UUID field "schema" to "version", according to changes in the
  specification.
- RGA: sequential UUIDs on initialization.
- Optimized `Base64.isLetter`.
- Extend `UUID.mkName` to accept any monad.
- Renamed `MonadStorage` methods `list...` -> `get...`
- Renamed `RON.Storage.saveDocument` -> `createDocument`

### Removed
- `RON.Storage.uuidToFileName` as it has no sense as an abstraction
- `RON.Storage.IO.runStorageT` with `StorageT`

## [0.1] - 2018-11-08
### Added
- Package `ron`
  - RON-text format
  - RON-binary format
  - RON-RDT:
    - LWW
    - RGA
    - OR-Set
    - VersionVector
  - RON-Schema
  - RON-Schema TemplateHaskell code generator

[Unreleased]: https://github.com/ff-notes/ron/compare/ron-schema-0.9...HEAD
[0.9]: https://github.com/ff-notes/ron/compare/ron-schema-0.8...ron-schema-0.9
[0.8]: https://github.com/ff-notes/ron/compare/ron-schema-0.7...ron-schema-0.8
[0.7]: https://github.com/ff-notes/ron/compare/ron-schema-0.6...ron-schema-0.7
[0.6]: https://github.com/ff-notes/ff/compare/v0.5...ron-schema-0.6
[0.5]: https://github.com/ff-notes/ff/compare/v0.4...v0.5
[0.4]: https://github.com/ff-notes/ff/compare/v0.3...v0.4
[0.3]: https://github.com/ff-notes/ff/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ff/compare/v0.1...v0.2
[0.1]: https://github.com/ff-notes/ff/tree/v0.1
