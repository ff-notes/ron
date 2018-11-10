# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Added
- Schema:
  - boole type
- RON.Storage and submodules are moved from ff project.

### Changed
- Renamed UUID field "schema" to "version", according to changes in the
  specification.

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

[Unreleased]: https://github.com/ff-notes/ff/compare/v0.1...HEAD
[0.1]: https://github.com/ff-notes/ff/tree/v0.1
