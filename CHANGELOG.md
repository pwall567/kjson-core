# Change Log

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [5.0] - 2022-11-27
### Added
- `JSONIncorrectTypeException`: contains details of incorrect type conversion
### Changed
- `JSONStructure`: added parameter for type of key (breaking change for some), added `getXxxx()` functions
- `JSONArray`, `JSONObject`: changed to use parameter on `JSONStructure`
- `JSON`: changed `asXxxx` properties to use `JSONIncorrectTypeException`

## [4.4] - 2022-11-24
### Changed
- `pom.xml`: updated dependency versions

## [4.3] - 2022-11-22
### Changed
- `pom.xml`: updated dependency versions

## [4.2] - 2022-11-20
### Changed
- `pom.xml`: updated dependency versions

## [4.1] - 2022-11-07
### Changed
- `JSONArray`: fixed possible bug in object creation
- `JSONArray`, `JSONObject`: added `@Suppress` for known warnings
- `pom.xml`: updated dependency versions

## [4.0] - 2022-10-03
### Changed
- `JSONNumber`: renamed from `JSONNumberValue` (breaking change for some)
- `JSONNumber`: now also implements `JSONValue`
- `JSONStructure`: added `isNotEmpty()`
- `JSON`: added `maxString` parameter to `displayValue()`

## [3.2] - 2022-09-19
### Changed
- `JSON`: added `elidedValue()` function

## [3.1] - 2022-09-04
### Added
- `ParseOptions`: options to control lenient parsing
### Changed
- `JSON`: added `parseNonNull()` function
- `Parser`: added lenient parsing of duplicate keys
- `JSONObject`: added ability to modify `Builder`
- `Parser`: added lenient parsing of quotes around object keys
- `Parser`: added lenient parsing of trailing commas

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
