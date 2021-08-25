# Change Log

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [1.2] - 2021-08-24
### Changed
- `pom.xml`: switched to `int-output` library

## [1.1] - 2021-08-24
### Changed
- `pom.xml`: bumped dependency versions

## [1.0] - 2021-08-21
### Changed
- `JSONObject`, `JSONArray`: added `build` functions
- `JSONDecimal`: added constructors from Int, Long
- `JSONInt`, `JSONLong`, `JSONDecimal`: implement `JSONNumberValue` functions
- `JSON`: added `asString`, `asInt`, `asLong`, `asDecimal`, `asBoolean`, `asArray`, `asObject`
### Added
- `JSONNumberValue`: abstract class to help conversion between number types

## [0.2] - 2021-08-02
### Changed
- `JSONBoolean`: changed `JSONBoolean` to an `enum class`
- `README.md`: added documentation

## [0.1] - 2021-08-02
### Added
- all files: initial versions (work in progress)
