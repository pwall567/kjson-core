# Change Log

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [7.0] - 2024-02-10
### Added
- `JSONProperty`: `typealias` for the `JSONObject` map entry
### Changed
- `JSONObject`: changed to also implement `List` (as well as `Map`) to allow simple iteration over properties

## [6.2] - 2024-01-01
### Changed
- `JSONException`: ensure `message` is always non-null

## [6.1] - 2023-12-20
### Changed
- `JSON`: simplified `displayValue()` function

## [6.0] - 2023-12-19
### Changed
- `JSONException`, `JSONIncorrectTypeException` (renamed `JSONTypeException`): simplified exceptions

## [5.11] - 2023-12-02
### Changed
- `pom.xml`: updated dependency versions

## [5.10] - 2023-09-24
### Changed
- `JSONArray`: switched to use Kotlin intrinsic functions
- `JSONDecimal`: minor simplification of `equals()`
- `JSON`: simplified `appendElided()`
- `pom.xml`: updated dependency versions
- many files: Added more KDoc

## [5.9] - 2023-07-24
### Changed
- many files: Added KDoc
- `pom.xml`: updated dependency versions

## [5.8] - 2023-07-22
### Changed
- `pom.xml`: updated Kotlin version to 1.8.22

## [5.7] - 2023-06-21
### Added
- `AbstractBuilder`: generic base for `Builder` classes in `JSONArray` and `JSONObject`
### Changed
- `Parser`, `JSON`: added handling of JSON Lines
- `JSONArray`, `JSONObject`: optimised `Builder` classes
- `pom.xml`: updated parent POM version
- `diagram.dia`, `diagram.png`: added new functions
- `Parser`, `ParseOptions`: added check for excessive nesting
- `JSONArray`, `JSONObject`: changed to use `AbstractBuilder`
- `pom.xml`: updated dependency version

## [5.6] - 2023-04-23
### Changed
- `pom.xml`: updated dependency versions

## [5.5] - 2023-04-12
### Added
- `ParserConstants`, `ParserErrors`: split out from `Parser` to allow access from `kjson-stream`
- `LookupSet`: to allow definitions to be shared with `kjson-stream`
### Changed
- `JSON`: added `asXxxxOrError()` functions
- `JSONStructure`: switched to use `asXxxxOrError()` functions
- `ParseOptions`: added `DEFAULT`
- `pom.xml`: updated Kotlin version to 1.7.21
- `README.md`: expanded documentation

## [5.4] - 2023-01-08
### Changed
- `JSONArray`, `JSONObject`: added optimisations
- `JSONArray`: added `forEachItem()`
- `JSONObject`: added `forEachEntry()`, `forEachKey()`, `forEachValue()`
- `pom.xml`: updated dependency versions

## [5.3] - 2023-01-03
### Changed
- `JSON`: added array handling to `elidedValue()`

## [5.2] - 2022-12-16
### Changed
- `JSONObject`, `JSONArray`: added deprecations for `JSONObject.asObject` and `JSONArray.asArray`
- `JSONBoolean`, `JSONDecimal`, `JSONInt`, `JSONLong`, `JSONString`: added optimisations for `asXxxx`
- `JSONIncorrectTypeException`, `JSON`, `JSONStructure`: generalised exception to be more usable from other packages

## [5.1] - 2022-12-02
### Changed
- `JSON`: made `typeError()` function public (for use from `kjson-pointer`)
- `JSONIncorrectTypeException`: clarified and standardised error message

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
