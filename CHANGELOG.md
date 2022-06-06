# Change Log

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [3.0] - 2022-06-06
### Changed
- `JSON`, `JSONArray`, `JSONBoolean`, `JSONDecimal`, `JSONInt`, `JSONLong`, `JSONObject`, `JSONString`, `JSONValue`:
  added `output` functions and non-blocking `coOutput` functions
- `pom.xml`: bumped dependency versions

## [2.5] - 2022-05-29
### Changed
- `pom.xml`: bumped dependency versions

## [2.4] - 2022-05-01
### Changed
- `Parser`: minor improvement
- `pom.xml`: bumped dependency versions

## [2.3] - 2022-04-18
### Added
- `JSONStructure`, `JSONPrimitive`: additional interfaces
### Changed
- `JSONString`, `JSONInt`, `JSONLong`, `JSONDecimal`, `JSONBoolean`: implement `JSONPrimitive`
- `JSONArray`, `JSONObject`: implement `JSONStructure`
- `JSONString`: `subSequence()` now returns `JSONString` instead of `CharSequence`
- `JSONArray`: `subList()` now returns `JSONArray` instead of `List`
- `JSONArray`, `JSONObject`: minor optimisation in `appendTo()`
- `JSONInt`, `JSONLong`, `JSONDecimal`: take advantage of sealed class `JSONNumberValue`
- `README.md`, `diagram.dia`, `diagram.png`: expanded documentation
- `pom.xml`: bumped dependency versions

## [2.2] - 2022-01-28
### Changed
- `JSONNumberValue`: changed to sealed class

## [2.1] - 2022-01-27
### Changed
- `JSON`: added more unsigned integer extension functions
- `pom.xml`: bumped dependency versions

## [2.0] - 2022-01-24
### Changed
- `JSONNumberValue`, `JSONInt`, `JSONLong`, `JSONDecimal`: added support for unsigned integers

## [1.5.1] - 2022-01-22
### Changed
- `pom.xml`: bumped dependency version

## [1.5] - 2022-01-21
### Changed
- `pom.xml`: bumped dependency versions
- `JSONObject`: modified for change to `immutables`

## [1.4] - 2021-10-13
### Changed
- `JSONObject`: added `containsKey()` to `Builder`
- `JSONObject`, `JSONArray`: optimised builder when length = 0
- `pom.xml`: bumped dependency versions

## [1.3] - 2021-08-24
### Changed
- `JSONNumberValue`, `JSONInt`, `JSONLong`, `JSONDecimal`: added functions (`isZero` etc.)
- `JSON`: added `of()` function for array and object
- `README.md`: expanded documentation

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
