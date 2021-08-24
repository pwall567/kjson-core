# kjson-core

[![Build Status](https://travis-ci.com/pwall567/kjson-core.svg?branch=main)](https://app.travis-ci.com/github/pwall567/kjson-core)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Kotlin](https://img.shields.io/static/v1?label=Kotlin&message=v1.5.20&color=7f52ff&logo=kotlin&logoColor=7f52ff)](https://github.com/JetBrains/kotlin/releases/tag/v1.5.20)
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

The `JSONValue` interface specifies two functions:
- `appendJSON()` &ndash; this appends the JSON text form of the object to a specified `Appendable` (when outputting
  JSON, it is more efficient to append to a single `Appendable`, as opposed to creating strings for each element)
- `toJSON()` &ndash; this outputs the value in syntactically-correct JSON (a default implementation makes use of the
  above `appendJSON()` function)

`JSONValue` is a sealed interface and the implementing classes are limited to:
- [`JSONString`](#jsonstring) &ndash; a string value
- [`JSONInt`](#jsonint) &ndash; a number value that fits in a 32-bit integer
- [`JSONLong`](#jsonlong) &ndash; a number value that fits in a 64-bit integer
- [`JSONDecimal`](#jsondecimal) &ndash; all other number values
- [`JSONBoolean`](#jsonboolean) &ndash; a boolean value
- [`JSONArray`](#jsonarray) &ndash; an array
- [`JSONObject`](#jsonobject) &ndash; an object

The implementing classes are all immutable.

### `JSONString`

The `JSONString` class holds a JSON string value.
The parser converts JSON escape sequences on input, and the `appendJSON()` and `toJSON()` functions convert non-ASCII
characters to escape sequences on output.

The `String` value may be accessed by the property `value` (which will never be `null`).

`JSONString` also implements the `CharSequence` interface, 

### `JSONInt`

The `JSONInt` class holds JSON number values that fit in a 32-bit integer.

The `Int` value may be accessed by the property `value`.

### `JSONLong`

The `JSONLong` class holds JSON number values that are too big for `JSONInt`, but will fit in a 64-bit long integer.

The `Long` value may be accessed by the property `value`.

### `JSONDecimal`

The `JSONDecimal` class holds all other JSON number values &ndash; integers that are too big for 64 bits, or numbers in
floating point representation.

The value is held as a `BigDecimal`, and this may be accessed by the property `value`.

### `JSONBoolean`

`JSONBoolean` is an `enum class` with two members &ndash; `TRUE` and `FALSE`.

The `Boolean` value may be accessed by the property `value`.

### `JSONArray`

The `JSONArray` class implements the `List<JSONValue?>` interface, and all the functions of that interface are available
to navigate the array (indexing via `array[n]`, `contains(obj)`, `iterator()` _etc._).

The constructor for `JSONArray` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow arrays to be constructed dynamically.

### `JSONObject`

The `JSONObject` class implements the `Map<String, JSONValue?>` interface, and all the functions of that interface are
available to navigate the object (retrieval via `array["name"]`, `containsKey("name")`, `entries` _etc._).
The original order of the input is maintained on parsing or on the programmatic creation of a `JSONObject`.

The constructor for `JSONObject` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow objects to be constructed dynamically.

### `JSON`

The `JSON` object contains a number of functions to assist with parsing and object creation.
The simplest way to parse JSON text is:
```kotlin
        val json = JSON.parse(text)
```

The result will be of type `JSONValue?` - it will be `null` if the text consists of just the string "`null`" (with
possible leading and trailing whitespace).

## Dependency Specification

The latest version of the library is 1.1, and it may be obtained from the Maven Central repository.

### Maven
```xml
    <dependency>
      <groupId>io.kjson</groupId>
      <artifactId>kjson-core</artifactId>
      <version>1.1</version>
    </dependency>
```
### Gradle
```groovy
    implementation "io.kjson:kjson-core:1.1"
```
### Gradle (kts)
```kotlin
    implementation("io.kjson:kjson-core:1.1")
```

Peter Wall

2021-08-24
