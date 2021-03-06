# kjson-core

[![Build Status](https://travis-ci.com/pwall567/kjson-core.svg?branch=main)](https://app.travis-ci.com/github/pwall567/kjson-core)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Kotlin](https://img.shields.io/static/v1?label=Kotlin&message=v1.6.10&color=7f52ff&logo=kotlin&logoColor=7f52ff)](https://github.com/JetBrains/kotlin/releases/tag/v1.6.10)
[![Maven Central](https://img.shields.io/maven-central/v/io.kjson/kjson-core?label=Maven%20Central)](https://search.maven.org/search?q=g:%22io.kjson%22%20AND%20a:%kjson-core%22)

JSON Kotlin core library

## Background

The input of JSON data generally consists of two main phases &ndash; parsing the input text and converting the
human-readable form into an easily navigated internal representation, and then mapping that internal form into
pre-existing data types.
Output may similarly use an intermediate form, but it is on the input side that the converted form is most useful
&ndash; it allows, for example, all of the properties of an object to be analysed before the determination of the
appropriate representation for the object.

There are also many types of JSON processing functions that do not require mapping to a target class &ndash; they simply
require an internal representation of the JSON data.

The `kjson-core` library provides the basic functionality required to represent JSON values in Kotlin, including:
- parsing functions to convert JSON text to a structure of JSON values
- classes to hold the internal forms of the values
- output functions to create valid JSON representations from the internal form

The library is an evolution of the [jsonutil](https://github.com/pwall567/jsonutil) Java library; it makes better use of
Kotlin-specific functionality like controlled nullability.

## User Guide

All JSON values are represented by Kotlin objects of type `JSONValue?` &ndash; that is, they are all instances of
classes that implement the `JSONValue` interface, or in the case of the JSON "`null`" value they are `null`.

### `JSONValue`

The `JSONValue` interface specifies four functions:
- `appendTo()` &ndash; this appends the JSON text form of the object to a specified `Appendable` (when outputting
  JSON, it is more efficient to append to a single `Appendable`, as opposed to creating strings for each element)
- `toJSON()` &ndash; this outputs the value in syntactically-correct JSON (a default implementation makes use of the
  above `appendTo()` function)
- `output()` &ndash; this outputs the JSON text form of the object using an `IntConsumer` (similar to `appendTo()`, but
  allowing a greater choice of output mechanism)
- `coOutput()` (suspend function) &ndash; non-blocking version of `output()`, suitable for use in a coroutine-based
  environment

`JSONValue` is a sealed interface and the implementing classes are limited to:
- [`JSONString`](#jsonstring) &ndash; a string value
- [`JSONInt`](#jsonint) &ndash; a number value that fits in a 32-bit signed integer
- [`JSONLong`](#jsonlong) &ndash; a number value that fits in a 64-bit signed integer
- [`JSONDecimal`](#jsondecimal) &ndash; all other number values
- [`JSONBoolean`](#jsonboolean) &ndash; a boolean value
- [`JSONArray`](#jsonarray) &ndash; an array
- [`JSONObject`](#jsonobject) &ndash; an object

The implementing classes are all immutable.

### `JSONPrimitive`

`JSONPrimitive` is a sealed interface (and a sub-interface of [`JSONValue`](#jsonvalue)) implemented by the classes for
primitive values, _i.e._ [`JSONInt`](#jsonint), [`JSONLong`](#jsonlong), [`JSONDecimal`](#jsondecimal),
[`JSONString`](#jsonstring) and [`JSONBoolean`](#jsonboolean).
It is a parameterised interface, where the parameter is the type of the value.
The interface specifies a single value (named `value`), of the parameter type.
The value is never `null`.

### `JSONNumberValue`

In addition to implementing [`JSONPrimitive`](#jsonprimitive), the number value classes all derive from the sealed class
`JSONNumberValue`, which itself derives from the system class `Number`.
That class provides a set of `toInt()`, `toLong()` _etc._ functions, to which `JSONNumberValue` adds the following:

Function      | Converts the value to...
--------------|-------------------------
`toDecimal()` | `BigDecimal`
`toUlong()`   | `ULong`
`toUInt()`    | `UInt`
`toUShort()`  | `UShort`
`toUByte()`   | `UByte`

`JSONNumberValue` also provides the following boolean functions:

Function          | Returns `true` iff...
------------------|-----------------------------------------------------------------
`isIntegral()`    | the value has no fractional part, or the fractional part is zero
`isLong()`        | the value may be converted to `Long` with no loss of precision
`isInt()`         | the value may be converted to `Int` with no loss of precision
`isShort()`       | the value may be converted to `Short` with no loss of precision
`isByte()`        | the value may be converted to `Byte` with no loss of precision
`isULong()`       | the value may be converted to `ULong` with no loss of precision
`isUInt()`        | the value may be converted to `UInt` with no loss of precision
`isUShort()`      | the value may be converted to `UShort` with no loss of precision
`isUByte()`       | the value may be converted to `UByte` with no loss of precision
`isZero()`        | the value is equal to 0
`isNegative()`    | the value is less than 0
`isPositive()`    | the value is greater than 0
`isNotZero()`     | the value is not equal to 0
`isNotNegative()` | the value is greater than or equal to 0
`isNotPositive()` | the value is less than or equal to 0

### `JSONInt`

The `JSONInt` class holds JSON number values that fit in a 32-bit signed integer.
The class derives from [`JSONNumberValue`](#jsonnumbervalue), providing implementations for all the abstract functions
of that class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `Int`.

The `Int` value may be accessed by the property `value`.

### `JSONLong`

The `JSONLong` class holds JSON number values that will fit in a 64-bit signed long integer.
The class derives from [`JSONNumberValue`](#jsonnumbervalue), providing implementations for all the abstract functions
of that class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `Long`.

The `Long` value may be accessed by the property `value`.

### `JSONDecimal`

The `JSONDecimal` class holds all other JSON number values &ndash; integers that are too big for 64 bits, or numbers in
floating point representation.
The class derives from [`JSONNumberValue`](#jsonnumbervalue), providing implementations for all the abstract functions
of that class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `BigDecimal`.

The `BigDecimal` value may be accessed by the property `value`.

### `JSONString`

The `JSONString` class holds a JSON string value.
The class implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `String`.

The parser converts JSON escape sequences on input, and the `appendJSON()` and `toJSON()` functions convert non-ASCII
characters to escape sequences on output.

The `String` value may be accessed by the property `value` (which will never be `null`).

`JSONString` also implements the `CharSequence` interface, which allows access to all the functionality of that
interface without having to extract the `value` property.
The `subSequence()` function will return a new `JSONString`.

### `JSONBoolean`

`JSONBoolean` is an `enum class` with two members &ndash; `TRUE` and `FALSE`.
The class implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `Boolean`.

The `Boolean` value may be accessed by the property `value`.

### `JSONStructure`

`JSONStructure` is a sealed interface (another sub-interface of [`JSONValue`](#jsonvalue)) implemented by the classes
for structured types, that is, arrays and objects.
It specifies a single value `size` (`Int`) which gives the number of entries in the array or object, and the function
`isEmpty()` which (unsurprisingly) returns `true` if the structure is empty.

### `JSONArray`

The `JSONArray` class implements the `List<JSONValue?>` interface, and all the functions of that interface are available
to navigate the array (indexing via `array[n]`, `contains(obj)`, `iterator()` _etc._).
The `subList()` function will return a new `JSONArray`.

The class also implements the [`JSONStructure`](#jsonstructure) interface.

The constructor for `JSONArray` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow arrays to be constructed dynamically.

### `JSONObject`

The `JSONObject` class implements the `Map<String, JSONValue?>` interface, and all the functions of that interface are
available to navigate the object (retrieval via `array["name"]`, `containsKey("name")`, `entries` _etc._).
The class also implements the [`JSONStructure`](#jsonstructure) interface.

The original order of the input is maintained on parsing or on the programmatic creation of a `JSONObject`.

The constructor for `JSONObject` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow objects to be constructed dynamically.

### `JSON`

The `JSON` object contains a number of functions to assist with parsing and object creation.
The simplest way to parse JSON text is:
```kotlin
        val json = JSON.parse(text)
```

The result will be of type `JSONValue?` &ndash; it will be `null` if the text consists of just the string "`null`" (with
possible leading and trailing whitespace).

If the JSON is expected to be an object (and it is an error if it is not):
```kotlin
        val json = JSON.parseObject(text)
```
In this case the result will be of type `JSONObject`, and an exception will be thrown if it is not an object.

Similarly, if the JSON is expected to be an array:
```kotlin
        val json = JSON.parseArray(text)
```
The result type will be `JSONArray`, and again, an exception will be thrown if the input is not of the correct type.

The `JSON` object also provides a number of shortcut functions to create `JSONValue`s:

Function                                   | Creates
------------------------------------------ | --------
`JSON.of(Int)`                             | `JSONInt`
`JSON.of(Long)`                            | `JSONLong`
`JSON.of(BigDecimal)`                      | `JSONDecimal`
`JSON.of(String)`                          | `JSONString`
`JSON.of(Boolean)`                         | `JSONBoolean`
`JSON.of(vararg JSONValue?)`               | `JSONArray`
`JSON.of(vararg Pair<String, JSONValue?>)` | `JSONObject`

To simplify casting a `JSONValue` to the expected type, the `JSON` object provides extension functions on `JSONValue?`:

Function                     | Result type   | If the value is not of that type...
---------------------------- | ------------- | -----------------------------------
`JSONValue?.asString`        | `String`      | throw exception
`JSONValue?.asStringOrNull`  | `String?`     | return `null`
`JSONValue?.asLong`          | `Long`        | throw exception
`JSONValue?.asLongOrNull`    | `Long?`       | return `null`
`JSONValue?.asInt`           | `Int`         | throw exception
`JSONValue?.asIntOrNull`     | `Int?`        | return `null`
`JSONValue?.asShort`         | `Short`       | throw exception
`JSONValue?.asShortOrNull`   | `Short?`      | return `null`
`JSONValue?.asByte`          | `Byte`        | throw exception
`JSONValue?.asByteOrNull`    | `Byte?`       | return `null`
`JSONValue?.asULong`         | `ULong`       | throw exception
`JSONValue?.asULongOrNull`   | `ULong?`      | return `null`
`JSONValue?.asUInt`          | `UInt`        | throw exception
`JSONValue?.asUIntOrNull`    | `UInt?`       | return `null`
`JSONValue?.asUShort`        | `UShort`      | throw exception
`JSONValue?.asUShortOrNull`  | `UShort?`     | return `null`
`JSONValue?.asUByte`         | `UByte`       | throw exception
`JSONValue?.asUByteOrNull`   | `UByte?`      | return `null`
`JSONValue?.asDecimal`       | `BigDecimal`  | throw exception
`JSONValue?.asDecimalOrNull` | `BigDecimal?` | return `null`
`JSONValue?.asBoolean`       | `Boolean`     | throw exception
`JSONValue?.asBooleanOrNull` | `Boolean?`    | return `null`
`JSONValue?.asArray`         | `JSONArray`   | throw exception
`JSONValue?.asArrayOrNull`   | `JSONArray?`  | return `null`
`JSONValue?.asObject`        | `JSONObject`  | throw exception
`JSONValue?.asObjectOrNull`  | `JSONObject?` | return `null`

Lastly, to simplify error reporting, the `JSON` object provides a `displayValue()` extension function on `JSONValue?` to
create an abbreviated form of the value suitable for error messages.

## Class Diagram

This class diagram may help to explain the main classes and interfaces and the relationships between them.

![Class Diagram](diagram.png "UML Class Diagram")

The diagram was produced by [Dia](https://wiki.gnome.org/Apps/Dia/); the diagram file is at [diagram.dia](diagram.dia).

## Dependency Specification

The latest version of the library is 3.0, and it may be obtained from the Maven Central repository.

### Maven
```xml
    <dependency>
      <groupId>io.kjson</groupId>
      <artifactId>kjson-core</artifactId>
      <version>3.0</version>
    </dependency>
```
### Gradle
```groovy
    implementation "io.kjson:kjson-core:3.0"
```
### Gradle (kts)
```kotlin
    implementation("io.kjson:kjson-core:3.0")
```

Peter Wall

2022-06-06
