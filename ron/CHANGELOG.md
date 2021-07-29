# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Added
- `RON.Text.Parse.parsePayload`

### Changed
- RON.Epoch:
  - Extend `EpochClock` to transformer `EpochClockT`

### Fixed
- RONt: parsing of `-0.1`

### Removed
- RON.Prelude:
  - atomicModifyIORef'
  - newIORef
  - readIORef
  - writeIORef
  because sometimes we need UnliftIO counterparts

## [0.12] - 2021-07-15
### Added
- Support for GHC 8.10.

### Fixed
- Internal parser helper (<+>)

## [0.11] - 2020-06-14
### Added
- Support for GHC 8.8.

## [0.10] - 2019-10-07
### Added
- Instances of `Generic` and `Hashable` for `ObjectRef`

## [0.9] - 2019-09-06
### Added
- A useful message when UUID parsing fails

### Changed
- Renamed `Object` to `ObjectRef`

### Removed
- Types `NetworkSim`, `ReplicaSim`, functions `runNetworkSim`, `runReplicaSim`.
  Use `NetworkSimT`, `ReplicaSimT`, functions `runNetworkSimT`, `runReplicaSimT`
  instead.

## [0.8] - 2019-08-10
### Added
- Type alias `Payload = [Atom]`
- Associated type `ReplicatedAsObject.Rep`
- Function `correct`

### Changed
- Renamed `ObjectState` to `ObjectFrame`.
- Simplified `Error` type
- Type `StateChunk` is split into two new:
  - `StateChunk` is tagged on Haskell-type-level with its RDT
  - `WireStateChunk` has RDT as a field, isomorphic to the old `StateChunk`

### Removed
- Method `ReplicatedAsObject.objectOpType`

## [0.7] - 2019-07-26
### Added
- Instance `ReplicaClock WriterT`.
- Instance `MonadFail ReplicaSimT`.
- Module `RON.Semilattice`:
  - Class `Semilattice`.
  - Class alias `BoundedSemilattice`.

### Changed
- Now `ObjectState` keeps a typed reference to an object with state frame
  attached,
  and `Object` is just a typed UUID --
  a typed reference to an object in a state frame passed in MonadState context.

### Removed
- `ObjectPart` as not used in RON-RDT 2.1.
- Support of space inside text UUID, as removed in RON 2.1.

## [0.6] - 2019-03-01
### Added
- `RON.Error`:
  - `liftEither`
  - `liftEitherString`

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

[Unreleased]: https://github.com/ff-notes/ron/compare/ron-0.12...HEAD
[0.12]: https://github.com/ff-notes/ron/compare/ron-0.11...ron-0.12
[0.11]: https://github.com/ff-notes/ron/compare/ron-0.10...ron-0.11
[0.10]: https://github.com/ff-notes/ron/compare/ron-0.9...ron-0.10
[0.9]: https://github.com/ff-notes/ron/compare/ron-0.8...ron-0.9
[0.8]: https://github.com/ff-notes/ron/compare/ron-0.7...ron-0.8
[0.7]: https://github.com/ff-notes/ron/compare/ron-0.6...ron-0.7
[0.6]: https://github.com/ff-notes/ron/compare/v0.5...ron-0.6
[0.5]: https://github.com/ff-notes/ron/compare/v0.4...v0.5
[0.4]: https://github.com/ff-notes/ron/compare/v0.3...v0.4
[0.3]: https://github.com/ff-notes/ron/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ron/compare/v0.1...v0.2
[0.1]: https://github.com/ff-notes/ron/tree/v0.1
