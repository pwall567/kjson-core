# kjson-core

[![Build Status](https://github.com/pwall567/kjson-core/actions/workflows/build.yml/badge.svg)](https://github.com/pwall567/kjson-core/actions/workflows/build.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Kotlin](https://img.shields.io/static/v1?label=Kotlin&message=v2.0.21&color=7f52ff&logo=kotlin&logoColor=7f52ff)](https://github.com/JetBrains/kotlin/releases/tag/v2.0.21)
[![Maven Central](https://img.shields.io/maven-central/v/io.kjson/kjson-core?label=Maven%20Central)](https://central.sonatype.com/artifact/io.kjson/kjson-core)

JSON Kotlin core library

## Background

The input of JSON data generally consists of two main phases:
1. parsing the input text and converting the human-readable form into an easily navigated internal structure, and
2. mapping that internal form into pre-existing data types.
Output may similarly use an intermediate form, but it is on the input side that the converted form is most useful
&ndash; it allows, for example, all of the properties of an object to be analysed before the determination of the
appropriate internal representation for the object.

There are also many types of JSON processing functions that do not require mapping to a target class &ndash; they simply
require an internal representation of the JSON data.

The `kjson-core` library provides the basic functionality required to represent JSON values in Kotlin, including:
- parsing functions to convert JSON text to a structure of JSON values
- classes to hold the internal forms of the values
- output functions to create valid JSON representations from the internal form

The library is an evolution of the [`jsonutil`](https://github.com/pwall567/jsonutil) Java library; it makes better use
of Kotlin-specific functionality like controlled nullability, and it adds functions to simplify the navigation of the
internal structure.

## User Guide

All JSON values are represented by Kotlin objects of type &ldquo;`JSONValue?`&rdquo; &ndash; that is, they are all
instances of classes that implement the `JSONValue` interface, or in the case of the JSON &ldquo;`null`&rdquo; value
they are `null`.

### `JSONValue`

The `JSONValue` interface specifies four functions:
- `appendTo()` &ndash; this appends the JSON text form of the object to a specified `Appendable`, _e.g._ a `Writer`
  (when outputting JSON, it is more efficient to append to a single `Appendable`, as opposed to creating strings for
  each element)
- `toJSON()` &ndash; this creates a `String` representation of the value in syntactically-correct JSON (a default
  implementation makes use of the above `appendTo()` function)
- `outputTo()` &ndash; this outputs the JSON text form of the value using an `IntConsumer` (similar to `appendTo()`, but
  allowing a greater choice of output mechanism)
- `coOutputTo()` (suspend function) &ndash; non-blocking version of `outputTo()`, suitable for use in a coroutine-based
  environment

`JSONValue` is a sealed interface and the implementing classes are limited to:
- [`JSONString`](#jsonstring) &ndash; a string value
- [`JSONInt`](#jsonint) &ndash; a number value that fits in a 32-bit signed integer
- [`JSONLong`](#jsonlong) &ndash; a number value that fits in a 64-bit signed integer
- [`JSONDecimal`](#jsondecimal) &ndash; any number value, including non-integer (uses `BigDecimal` internally)
- [`JSONBoolean`](#jsonboolean) &ndash; a boolean value
- [`JSONArray`](#jsonarray) &ndash; an array
- [`JSONObject`](#jsonobject) &ndash; an object

The implementing classes are all immutable.

Following a common Kotlin pattern, there are creation functions named `JSONValue` which create the appropriate
implementing class depending on the parameter type:

| Parameter Type                    | Result                        |
|-----------------------------------|-------------------------------|
| `String`                          | [`JSONString`](#jsonstring)   |
| `Int`                             | [`JSONInt`](#jsonint)         |
| `Long`                            | [`JSONLong`](#jsonlong)       |
| `BigDecimal`                      | [`JSONDecimal`](#jsondecimal) |
| `Boolean`                         | [`JSONBoolean`](#jsonboolean) |
| `vararg JSONValue?`               | [`JSONArray`](#jsonarray)     |
| `vararg Pair<String, JSONValue?>` | [`JSONObject`](#jsonobject)   |
| `vararg JSONObject.Property`      | [`JSONObject`](#jsonobject)   |

### `JSONPrimitive`

`JSONPrimitive` is a sealed interface (and a sub-interface of [`JSONValue`](#jsonvalue)) implemented by the classes for
primitive values, _i.e._ [`JSONInt`](#jsonint), [`JSONLong`](#jsonlong), [`JSONDecimal`](#jsondecimal),
[`JSONString`](#jsonstring) and [`JSONBoolean`](#jsonboolean).
It is a parameterised interface, where the parameter is the type of the value.
The interface specifies a single value (named `value`), of the parameter type.
The value is never `null`.

### `JSONNumber`

In addition to implementing [`JSONPrimitive`](#jsonprimitive), the number value classes [`JSONInt`](#jsonint),
[`JSONLong`](#jsonlong) and [`JSONDecimal`](#jsondecimal) all derive from the sealed class `JSONNumber`, which itself
derives from the system class `kotlin.Number`.
This means that these classes may be used without conversion anywhere a `Number` is called for.

The `Number` class provides a set of `toInt()`, `toLong()` _etc._ functions, to which `JSONNumber` adds the following:

| Function      | Converts the value to... |
|---------------|--------------------------|
| `toDecimal()` | `BigDecimal`             |
| `toULong()`   | `ULong`                  |
| `toUInt()`    | `UInt`                   |
| `toUShort()`  | `UShort`                 |
| `toUByte()`   | `UByte`                  |

`JSONNumber` also provides the following boolean functions:

| Function          | Returns `true` iff...                                            |
|-------------------|------------------------------------------------------------------|
| `isIntegral()`    | the value has no fractional part, or the fractional part is zero |
| `isLong()`        | the value may be converted to `Long` with no loss of precision   |
| `isInt()`         | the value may be converted to `Int` with no loss of precision    |
| `isShort()`       | the value may be converted to `Short` with no loss of precision  |
| `isByte()`        | the value may be converted to `Byte` with no loss of precision   |
| `isULong()`       | the value may be converted to `ULong` with no loss of precision  |
| `isUInt()`        | the value may be converted to `UInt` with no loss of precision   |
| `isUShort()`      | the value may be converted to `UShort` with no loss of precision |
| `isUByte()`       | the value may be converted to `UByte` with no loss of precision  |
| `isZero()`        | the value is equal to 0                                          |
| `isNegative()`    | the value is less than 0                                         |
| `isPositive()`    | the value is greater than 0                                      |
| `isNotZero()`     | the value is not equal to 0                                      |
| `isNotNegative()` | the value is greater than or equal to 0                          |
| `isNotPositive()` | the value is less than or equal to 0                             |

The `JSONNumber` classes also override `equals()` (and `hashCode()`) so that instances with the same value but different
types will be regarded as equal.
`JSONInt(27)`, `JSONLong(27)` and `JSONDecimal("27.0")` will all be considered equal, and will all return the same hash
code.

Following a common Kotlin pattern, there are creation functions named `JSONNumber` which create the appropriate
derived class depending on the parameter type:

| Parameter Type                    | Result                        |
|-----------------------------------|-------------------------------|
| `Int`                             | [`JSONInt`](#jsonint)         |
| `Long`                            | [`JSONLong`](#jsonlong)       |
| `BigDecimal`                      | [`JSONDecimal`](#jsondecimal) |

### `JSONInt`

The `JSONInt` class holds JSON number values that fit in a 32-bit signed integer.
The class derives from [`JSONNumber`](#jsonnumber), providing implementations for all the abstract functions of that
class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `Int`.

The `Int` value may be accessed by the property `value`.

### `JSONLong`

The `JSONLong` class holds JSON number values that will fit in a 64-bit signed long integer.
The class derives from [`JSONNumber`](#jsonnumber), providing implementations for all the abstract functions of that
class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `Long`.

The `Long` value may be accessed by the property `value`.

### `JSONDecimal`

The `JSONDecimal` class holds any JSON number values, including non-integer values.
The class derives from [`JSONNumber`](#jsonnumber), providing implementations for all the abstract functions of that
class, and it also implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `BigDecimal`.

The `BigDecimal` value may be accessed by the property `value`.

### `JSONString`

The `JSONString` class holds a JSON string value.
The class implements [`JSONPrimitive`](#jsonprimitive) with the parameter type `String`.

The parser converts JSON escape sequences on input, and the `appendJSON()` and `toJSON()` functions convert non-ASCII
characters to escape sequences on output.

The `String` value may be accessed by the property `value` (which will never be `null`).

A `JSONString` may be constructed dynamically using the `build {}` function, which takes as a lambda parameter an
extension function on `StringBuilder`; anything appended to the `StringBuilder` will become part of the `JSONString`.
For example:
```kotlin
    val jsonString = JSONString.build {
        append("Number = ")
        append(number)
    }
```

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
It specifies a single value `size` (`Int`) which gives the number of entries in the array or object, and the functions
`isEmpty()` and `isNotEmpty()` which (unsurprisingly) return `true` or `false` respectively if the structure is empty.

`JSONStructure` is a parameterised interface, where the parameter `K` is the type of the value to locate entries in the
structure, _i.e._ `Int` for `JSONArray` or `String` for `JSONObject`.

It also provides convenience functions to both get a member of the structure (using a key of the parameter type `K`) and
convert it to the required type:

| Function        | Converts the value to... |
|-----------------|--------------------------|
| `getString(K)`  | `String`                 |
| `getLong(K)`    | `Long`                   |
| `getInt(K)`     | `Int`                    |
| `getShort(K)`   | `Short`                  |
| `getByte(K)`    | `Byte`                   |
| `getULong(K)`   | `ULong`                  |
| `getUInt(K)`    | `UInt`                   |
| `getUShort(K)`  | `UShort`                 |
| `getUByte(K)`   | `UByte`                  |
| `getDecimal(K)` | `BigDecimal`             |
| `getBoolean(K)` | `Boolean`                |
| `getArray(K)`   | `JSONArray`              |
| `getObject(K)`  | `JSONObject`             |

These have the advantage over, for example, `json["property"].asString`, that the
[`JSONTypeException`](#jsontypeexception) thrown when the type is incorrect includes the key or index used to select the
item.

### `JSONArray`

The `JSONArray` class implements the `List<JSONValue?>` interface, and all the functions of that interface are available
to navigate the array (indexing via `array[n]`, `contains(obj)`, `iterator()` _etc._).
The `subList()` function will return a new `JSONArray`.

The class also implements the [`JSONStructure`](#jsonstructure) interface with a parameter type of `Int`.

The constructor for `JSONArray` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow arrays to be constructed dynamically.

Following a common Kotlin pattern, there is also a creation function named `JSONArray` taking a `vararg` array of
`JSONValue?`.

`JSONArray` implements the `equals()` and `hashCode()` functions as specified for the Java Collections classes, so that
an instance of `JSONArray` may be compared safely with an instance of any class correctly implementing
`List<JSONValue?>`.

The `JSONArray` class also provides optimised iteration functions to iterate over the items of an array.
Because the class implements the `List` interface, there are iteration functions available from the standard library,
but the additional functions are optimised for the specific implementation details of the `JSONArray` class.
```kotlin
    jsonArray.forEachItem {
        println("Item = $it")
    }
    jsonArray.forEachItemIndexed { index, item ->
        println("Item #$index = $item")
    }
```

### `JSONObject`

The `JSONObject` class implements the `Map<String, JSONValue?>` interface, and all the functions of that interface are
available to navigate the object (retrieval via `structure["name"]`, `containsKey("name")`, `entries` _etc._).
The class also implements the [`JSONStructure`](#jsonstructure) interface with a parameter type of `String`.

The original order of the input is maintained on parsing or on the programmatic creation of a `JSONObject`, and to take
advantage of this sequential ordering of properties, the `JSONObject` class also implements the `List<Property>`
interface, where `Property` is a nested class representing the `Map.Entry` objects used by `JSONObject` (see
[below](#jsonobjectproperty)).
This means that the `JSONObject` class provides both:
```kotlin
    jsonObject["name"]  // get the property named "name" as a JSONValue?
```
and:
```kotlin
    jsonObject[3] // get the fourth property (index 3) as a JSONObject.Property
 ```

The constructor for `JSONObject` is not publicly accessible, but an `of()` function is available in the
`companion object`, and a `build` function and the `Builder` nested class allow objects to be constructed dynamically.

Following a common Kotlin pattern, there are also creation functions named `JSONObject` taking a `vararg` array of
`Pair<String, JSONValue?>` or `JSONObject.Property`.

`JSONObject` implements the `equals()` and `hashCode()` functions as specified for the Java Collections classes, so that
an instance of `JSONObject` may be compared safely with an instance of any class correctly implementing
`Map<String, JSONValue?>`.

The `JSONObject` class also provides optimised iteration functions to iterate over the entries, the keys (property
names) or the values of an object.
Because the class implements the `Map` interface, there are iteration functions available from the standard library, but
the additional functions are optimised for the specific implementation details of the `JSONObject` class.
```kotlin
    jsonObject.forEachEntry { name, value ->
        println("Property $name = $value")
    }
    jsonObject.forEachProperty { property ->
        println("Property ${property.name} = ${property.value}")
    }
    jsonObject.forEachKey {
        println("Property name = $it")
    }
    jsonObject.forEachValue {
        println("Property value = $it")
    }
```

### `JSONObject.Property`

The `JSONObject.Property` nested class implements the `Map.Entry<String, JSONValue?>` interface, and is used to hold the
key-value pairs of the `Map` behind `JSONObject`,

It has two properties:

| Name    | Type         | Contains           |
|---------|--------------|--------------------|
| `name`  | `String`     | The property name  |
| `value` | `JSONValue?` | The property value |

The `JSONObject.Property` object is immutable.

There is an infix function `refersTo` taking a `String` and a `JSONValue?` which creates a `JSONObject.Property`:
```kotlin
    val property = "propertyName" refersTo JSONString("Property value")
```

### `JSONException`

Error conditions will usually result in a `JSONException` being thrown.
This is a derived class of `RuntimeException`, and the `message` property will contain a text description of the error.

The exception also includes a property `key` (of type `Any?`) which is used to provide information on the location of
the error, for example a [`JSONPointer`](https://github.com/pwall567/kjson-pointer) or a property name.
When the key is provided, it will be appended to the message, as &ldquo;`, at {key}`&rdquo;.

Starting from version 8.1 of this library, `JSONException` has been extracted to a separate library &ndash;
[`kjson-exception`](https://github.com/pwall567/kjson-exception) &ndash; so that it may be included in other projects
independent from this library

### `JSONTypeException`

A common error case arises when a `JSONValue` is found to be of the wrong type, for example, when a `JSONArray` is
supplied as a parameter to a function that expects a `JSONObject`, or when a property of an object is a `JSONString`
when a `JSONInt` was expected.
The `JSONTypeException` provides a way of reporting such errors in a consistent manner, with error messages including
the human-readable node name, the expected type, the actual value and an optional key (as described
[above](#jsonexception)).

The `JSONTypeException` constructor takes the following parameters, all of which are accessible as properties of the
exception object:

| Name       | Type         | Default  | Description                                         |
|------------|--------------|----------|-----------------------------------------------------|
| `nodeName` | `String`     | `"Node"` | The name of the field, _e.g._ `"Surname"`           |
| `expected` | `String`     |          | The expected type, _e.g._ `"string"`                |
| `value`    | `JSONValue?` |          | The actual value found                              |
| `key`      | `Any?`       | `null`   | The &ldquo;key&rdquo; (the location in a structure) |

For example, the following exception:
```kotlin
    throw JSONTypeException("Surname", "string", surname, "/person/surname")
```
will result in this message (if an array was supplied in place of a string):
```text
Surname not correct type (string), was [ ... ], at /person/surname
```

The actual value will be displayed using the [`displayValue()`](#human-friendly-output) function, and the
&ldquo;at&rdquo; clause will be omitted if the `key` is `null` or `key.toString()` returns an empty string.

For a more convenient way of using this exception type, see [Error Reporting](#error-reporting) below.

### `JSON`

The `JSON` object contains a number of functions to assist with parsing and object creation.

#### Parsing Functions

The simplest way to parse JSON text is:
```kotlin
        val json = JSON.parse(text)
```

The result will be of type `JSONValue?` &ndash; it will be `null` if the text consists of just the string
&ldquo;`null`&rdquo; (with possible leading and trailing whitespace).

If only non-null JSON values are expected:
```kotlin
        val json = JSON.parseNonNull(text)
```
The result of this function will be of type `JSONValue` (no question mark) and an exception will be thrown if the JSON
was &ldquo;`null`&rdquo;.

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

#### `JSONValue` Creation Functions

The `JSON` object also provides a number of shortcut functions to create `JSONValue`s:

| Function                                   | Creates       |
|--------------------------------------------|---------------|
| `JSON.of(Int)`                             | `JSONInt`     |
| `JSON.of(Long)`                            | `JSONLong`    |
| `JSON.of(BigDecimal)`                      | `JSONDecimal` |
| `JSON.of(String)`                          | `JSONString`  |
| `JSON.of(Boolean)`                         | `JSONBoolean` |
| `JSON.of(vararg JSONValue?)`               | `JSONArray`   |
| `JSON.of(vararg Pair<String, JSONValue?>)` | `JSONObject`  |

#### Human-Friendly Output

To simplify error reporting, the `JSON` object provides a `displayValue()` extension function on `JSONValue?` to create
an abbreviated form of the value suitable for error messages.

| Parameter   | Type  | Default | Purpose                                         |
|-------------|-------|--------:|-------------------------------------------------|
| `maxString` | `Int` |      21 | limit the number of string characters displayed |
| `maxArray`  | `Int` |       0 | limit the number of array items displayed       |
| `maxObject` | `Int` |       0 | limit the number of object properties displayed |

When displaying array items, the function will by default display `[ ... ]`, but when the `maxArray` parameter is
supplied with a positive value, the specified number of array items will be displayed.
If there are more values than specified in `maxArray`, the excess parameters are elided and `, ...` will be displayed.

For example:
```kotlin
    val array = JSONArray.build {
        add(123)
        add(456)
        add(789)
    }
    array.displayValue()                // displays [ ... ]
    array.displayValue(maxArray = 1)    // displays [ 123, ... ]
    array.displayValue(maxArray = 2)    // displays [ 123, 456, ... ]
    array.displayValue(maxArray = 3)    // displays [ 123, 456, 456 ]
```

Similarly, when displaying objects, the function will by default display `{ ... }`, and the use of the `maxObject`
parameter may be used to control the display of a specified number of properties.

For example:
```kotlin
    val obj = JSONObject.build {
        add("alpha", 123)
        add("beta", 456)
        add("gamma", 789)
    }
    obj.displayValue()                  // displays { ... }
    obj.displayValue(maxObject = 1)     // displays { "alpha": 123, ... }
    obj.displayValue(maxObject = 2)     // displays { "alpha": 123, "beta": 456, ... }
    obj.displayValue(maxObject = 3)     // displays { "alpha": 123, "beta": 456, "gamma": 789 }
```

Long strings are shortened with &ldquo;` ... `&rdquo; in the middle.
For example:
```kotlin
    JSONString("the quick brown fox jumps over the lazy dog").displayValue()
```
will display:
```
"the quic ... lazy dog"
```
The maximum number of characters to display in a string defaults to 21, but may be specified as a parameter, _e.g._
`displayValue(maxString = 17)` (odd numbers are best, because they result in the same number of characters before and
after the elision).

When nested array items or object properties are included, they are displayed using the same function recursively, but
with the `MaxString`, `maxArray` and `maxObject` values all halved.
So, for example, when `maxObject` is specified as 4, nested objects are displayed with `maxObject=2`, and next-level
nested objects are displayed with `maxOnject = 1`.

Internally, the `displayString()` function uses an `appendDisplayString()` extension function on `Appendable` to avoid
having to create a large number of small strings.
This function is also available for public use, with the following parameters.

| Parameter   | Type         | Default | Purpose                                         |
|-------------|--------------|--------:|-------------------------------------------------|
| `json`      | `JSONValue?` |         | the JSON value (or `null`)                      |
| `maxString` | `Int`        |      21 | limit the number of string characters displayed |
| `maxArray`  | `Int`        |       0 | limit the number of array items displayed       |
| `maxObject` | `Int`        |       0 | limit the number of object properties displayed |

For example:
```kotlin
    val sb = StringBuilder()
    sb.appendDisplayString(obj, maxArray = 4, maxObject = 2)
```

#### Security-Aware Output

There is often a requirement to log JSON inputs for later error diagnosis, with the restriction that logs must not
contain sensitive information.
The `elidedValue()` extension function on `JSONValue?` allows JSON values to be converted to the text form with certain
nominated elements excluded.

For example:
```kotlin
    val json = JSON.parse("""{"name":"Adam","accountNo":"12345678"}""")
    json.elidedValue(exclude = setOf("accountNo"))
```
will display:
```
{"name":"Adam","accountNo":"****"}
```
All elements with the specified name will be elided, wherever they occur in the object tree.

The elements to be elided may be specified as a `Collection` of element names to be excluded as shown above, or (less
usefully) as a `Collection` of element names to be included (using the `include` parameter).
The substitute string (default &ldquo;`****`&rdquo;) may also be specified using the `substitute` parameter.

#### Error Reporting

To simplify the creation of a `JSONTypeException`, the `JSON` object includes the `typeError()` extension function on
`JSONValue?`.
It takes the following parameters:

| Name       | Type     | Default  | Description                                         |
|------------|----------|----------|-----------------------------------------------------|
| `expected` | `String` |          | The expected type, _e.g._ `"string"`                | 
| `key`      | `Any?`   | `null`   | The &ldquo;key&rdquo; (the location in a structure) |
| `nodeName` | `String` | `"Node"` | The name of the field, _e.g._ `"Surname"`           |

Its use is best illustrated by example:
```kotlin
    if (node !is JSONString)
        node.typeError("String", "/person/surname", "Surname")
```
This will produce an exception like the one shown in the description of [`JSONTypeException`](#jsontypeexception).

The conversion may be combined with the error reporting using the `asXxxxOrError()` functions:

| Extension Function              | Result type  |
|---------------------------------|--------------|
| `JSONValue?.asStringOrError()`  | `String`     |
| `JSONValue?.asLongOrError()`    | `Long`       |
| `JSONValue?.asLongOrError()`    | `Long`       |
| `JSONValue?.asIntOrError()`     | `Int`        |
| `JSONValue?.asShortOrError()`   | `Short`      |
| `JSONValue?.asByteOrError()`    | `Byte`       |
| `JSONValue?.asULongOrError()`   | `ULong`      |
| `JSONValue?.asUIntOrError()`    | `UInt`       |
| `JSONValue?.asUShortOrError()`  | `UShort`     |
| `JSONValue?.asUByteOrError()`   | `UByte`      |
| `JSONValue?.asDecimalOrError()` | `BigDecimal` |
| `JSONValue?.asNumberOrError()`  | `Number`     |
| `JSONValue?.asBooleanOrError()` | `Boolean`    |
| `JSONValue?.asArrayOrError()`   | `JSONArray`  |
| `JSONValue?.asObjectOrError()`  | `JSONObject` |

Note that the functions representing a simple value return the actual value type, not the `JSONValue` subtype.
The `asArrayOrError()` and `asObjectOrError()` functions return `JSONArray` and `JSONObject` respectively, but these can
of course be used as the underlying implementation types (`List` and `Map`).

The functions all take the same parameters as the `typeError()` function (which they all call if the type is not
correct), but in the case of these functions, the `expected` parameter also has a default value, a string representing
the expected type.

Using these functions, the above example (for the use of `typeError`) may be written:
```kotlin
    node.asStringOrError(key = "/person/surname", nodeName = "Surname")
```

#### Extension Values

To simplify casting a `JSONValue` to the expected type, the `JSON` object provides extension values on `JSONValue?`:

| Extension Value              | Result type   | If the value is not of that type... |
|------------------------------|---------------|-------------------------------------|
| `JSONValue?.asString`        | `String`      | throw `JSONTypeException`           |
| `JSONValue?.asStringOrNull`  | `String?`     | return `null`                       |
| `JSONValue?.asLong`          | `Long`        | throw `JSONTypeException`           |
| `JSONValue?.asLongOrNull`    | `Long?`       | return `null`                       |
| `JSONValue?.asInt`           | `Int`         | throw `JSONTypeException`           |
| `JSONValue?.asIntOrNull`     | `Int?`        | return `null`                       |
| `JSONValue?.asShort`         | `Short`       | throw `JSONTypeException`           |
| `JSONValue?.asShortOrNull`   | `Short?`      | return `null`                       |
| `JSONValue?.asByte`          | `Byte`        | throw `JSONTypeException`           |
| `JSONValue?.asByteOrNull`    | `Byte?`       | return `null`                       |
| `JSONValue?.asULong`         | `ULong`       | throw `JSONTypeException`           |
| `JSONValue?.asULongOrNull`   | `ULong?`      | return `null`                       |
| `JSONValue?.asUInt`          | `UInt`        | throw `JSONTypeException`           |
| `JSONValue?.asUIntOrNull`    | `UInt?`       | return `null`                       |
| `JSONValue?.asUShort`        | `UShort`      | throw `JSONTypeException`           |
| `JSONValue?.asUShortOrNull`  | `UShort?`     | return `null`                       |
| `JSONValue?.asUByte`         | `UByte`       | throw `JSONTypeException`           |
| `JSONValue?.asUByteOrNull`   | `UByte?`      | return `null`                       |
| `JSONValue?.asDecimal`       | `BigDecimal`  | throw `JSONTypeException`           |
| `JSONValue?.asDecimalOrNull` | `BigDecimal?` | return `null`                       |
| `JSONValue?.asNumber`        | `Number`      | throw `JSONTypeException`           |
| `JSONValue?.asNumberOrNull`  | `Number?`     | return `null`                       |
| `JSONValue?.asBoolean`       | `Boolean`     | throw `JSONTypeException`           |
| `JSONValue?.asBooleanOrNull` | `Boolean?`    | return `null`                       |
| `JSONValue?.asArray`         | `JSONArray`   | throw `JSONTypeException`           |
| `JSONValue?.asArrayOrNull`   | `JSONArray?`  | return `null`                       |
| `JSONValue?.asObject`        | `JSONObject`  | throw `JSONTypeException`           |
| `JSONValue?.asObjectOrNull`  | `JSONObject?` | return `null`                       |

The [`JSONTypeException`](#jsontypeexception) will use the default value `"Node"` for the `nodeName`, and the class name
of the expected type as the default for `expected`.
The default value for `key` is `null`.

As with the `asXxxxOrError()` functions, the extension values representing a simple value return the actual value type,
not the `JSONValue` subtype (_i.e._ `asInt` returns `Int`, not `JSONInt`), but the `asArrayOrError()` and
`asObjectOrError()` functions return `JSONArray` and `JSONObject` respectively.

#### Extension Functions

A further way of casting a `JSONValue` to the expected type is provided by the `asXxxxOr` functions:

| Extension Function         | Result type  |
|----------------------------|--------------|
| `JSONValue?.asStringOr()`  | `String`     |
| `JSONValue?.asLongOr()`    | `Long`       |
| `JSONValue?.asIntOr()`     | `Int`        |
| `JSONValue?.asShortOr()`   | `Short`      |
| `JSONValue?.asByteOr()`    | `Byte`       |
| `JSONValue?.asULongOr()`   | `ULong`      |
| `JSONValue?.asUIntOr()`    | `UInt`       |
| `JSONValue?.asUShortOr()`  | `UShort`     |
| `JSONValue?.asUByteOr()`   | `UByte`      |
| `JSONValue?.asDecimalOr()` | `BigDecimal` |
| `JSONValue?.asNumberOr()`  | `Number`     |
| `JSONValue?.asBooleanOr()` | `Boolean`    |
| `JSONValue?.asArrayOr()`   | `JSONArray`  |
| `JSONValue?.asObjectOr()`  | `JSONObject` |

The functions all take a single parameter &ndash; a lambda with the `JSONValue?` as the receiver which will be invoked
if the `JSONValue?` is not the correct type.
This may be used to provide a default value, silently ignoring the type error, but more commonly it will be used to
throw an exception.
For example:
```kotlin
    node.asStringOr { typeError(expected = "string", key = "/person/surname", nodeName = "Surname") }
```

The advantage of using these functions as compared to `asXxxxOrError()`, is that these functions are inline, and the
code to set up the parameters and call a separate function will not be executed if the node is of the correct type.

Again, as with the `asXxxxOrError()` functions, the extension functions returning a simple value return the actual value
type, not the `JSONValue` subtype, and the `asArrayOr()` and `asObjectOr()` functions return `JSONArray` and
`JSONObject` respectively.

## JSON Lines

The [JSON Lines](https://jsonlines.org/) specification allows multiple JSON values to be specified in a single stream of
data, separated by newline (`\u000a`) characters.
For example, events may be logged to a file as a sequence of objects on separate lines; the alternative would be to
output a JSON array, but this would require a &ldquo;`]`&rdquo; terminator, complicating the shutdown of the process
(particularly abnormal shutdown).

```json lines
{"time":"2023-06-24T12:24:10.321+10:00","eventType":"ACCOUNT_OPEN","accountNumber": "123456789"}
{"time":"2023-06-24T12:24:10.321+10:00","eventType":"DEPOSIT","accountNumber": "123456789","amount":"1000.00"}
```

The individual items are usually objects (or sometimes arrays) formatted similarly, but that is not a requirement
&ndash; the items may be of any JSON type.

The JSON Lines format is particularly suitable for streaming data, so the
[`kjson-stream`](https://github.com/pwall567/kjson-stream) library is more likely to be useful for JSON Lines input than
the functions in this library that parse a complete file, but the functions here are provided for completeness.

### Parsing JSON Lines

The `kjson-core` library includes functions to parse JSON Lines format:
```kotlin
    val lines = JSON.parseLines(multiLineString)
```
The result will always be a `JSONArray`; an empty string will result in a zero-length array.

While the `parseLines()` function (and its corresponding function in the `Parser` class) will correctly parse a stream
of data in JSON Lines format, the newline separator is in fact not required.
The function will accept JSON objects and/or arrays concatenated without any delimiters, but because whitespace is
allowed between tokens of JSON data, the newline (if present) will be ignored.

### Output JSON Lines

In most cases, JSON Lines data will be output as individual objects using `appendTo()` or `toJSON()`.
If an entire `JSONArray` is required to be output in JSON Lines format, there are four additional functions for this
purpose:
- `appendJSONLinesTo()` &ndash; this appends the JSON Lines form of the `JSONArray` to a specified `Appendable`, _e.g._
  a `Writer`
- `toJSONLinesTo()` &ndash; this converts the `JSONArray` to a `String` in JSON Lines format
- `outputJSONLinesTo()` &ndash; this outputs the JSON Lines form of the `JSONArray` using an `IntConsumer` (similar to
  `appendJSONLinesTo()`, but allowing a greater choice of output mechanism)
- `coOutputJSONLinesTo()` (suspend function) &ndash; non-blocking version of `outputJSONLinesTo()`, suitable for use in
  a coroutine-based environment

The functions all add a single newline after each item in the `JSONArray` for human readability reasons, even though (as
noted above) this is not strictly necessary.

## `ParseOptions`

The parser will by default apply strict validation to the JSON input, and in some cases this may be unhelpful.
There is occasionally a need to parse JSON that is not correctly formatted according to the specification, particularly
for human-edited JSON, as opposed to JSON output by an automated process.
Also, for human-edited JSON, the ability to add comments is sometimes seen as desirable.

To accommodate these requirements, and to allow the specification of a maximum nesting depth, the parser may be supplied
with a `ParseOptions` object.
For example:
```kotlin
    val options = ParseOptions(
        objectKeyDuplicate = JSONObject.DuplicateKeyOption.ERROR,
        objectKeyUnquoted = false,
        objectTrailingComma = false,
        arrayTrailingComma = false,
        maximumNestingDepth = 1000,
        slashSlashComment = false,
        slashStarComment = false,
    )
    val jsonValue = Parser.parse(jsonString, options)
```

The `parseOptions` parameter is available on most functions invoking the `Parser`.
All of the properties of the object have default values, so only the properties that differ from their defaults need be
specified.

### `objectKeyDuplicate`

The JSON specification states that a given key **SHOULD** appear only once in an object, but some software may output
objects with the same key repeated multiple times.
Under normal circumstances, the parser will throw an exception when it encounters a second occurrence of the same key,
but if such data is required to be accepted, the `objectKeyDuplicate` options setting may be used to specify the desired
behaviour.

The field is an `enum` (`JSONObject.DuplicateKeyOption`), and the possible values are:

- `ERROR`: treat the duplicate key as an error (this is the default)
- `TAKE_FIRST`: take the value of the first occurrence and ignore duplicates
- `TAKE_LAST`: take only the last occurrence and ignore any preceding occurrences
- `CHECK_IDENTICAL`: ignore duplicates only if they are identical to the original value, otherwise report an error

### `objectKeyUnquoted`

Unlike JavaScript, on which it is based, JSON requires that object keys be enclosed in quotes.
Sometimes, particularly when parsing human-edited JSON, it can be a helpful to allow keys to be conventional computer
language identifiers, and this can be selected by the `objectKeyUnquoted` option.

Setting this flag to `true` will cause the parser to allow object keys to be specified without quotes.
When using this option, the keys must follow this pattern:

- the first character must be ASCII alphabetic (upper or lower case) or underscore
- subsequent characters must be ASCII alphabetic (upper or lower case) or numeric or underscore

### `objectTrailingComma`

When outputting the members of an object, it can be simpler to add a comma after each member, regardless of whether it
is the last one.
To allow trailing commas in objects, the option `objectTrailingComma` can be set to `true`.

### `arrayTrailingComma`

Similarly, when outputting the items of an array, it can be simpler to add a comma after each item.
To allow trailing commas in arrays, the option `arrayTrailingComma` can be set to `true`.

### `maximumNestingDepth`

Faulty JSON output software &ndash; or malicious actors &ndash; may sometimes cause a sequence of characters
representing very deep nesting of objects or arrays to be presented to the parser.
Very few uses of JSON will require more than about 100 levels of nesting, so a sequence of characters that cause more
than a few hundred levels of nesting are almost always the result of an error or a malicious attack.

The parser needs to allocate a small amount of memory for each level of nesting, and unlimited nesting would cause the
process to fail, so the parser limits the number of levels allowed.
The default maximum nesting level is 1000, which should be more than enough for most uses.
The limit may be increased (or decreased) by the use of the `maximumNestingDepth` option.

### `slashSlashComment`

When this option is set to `true`, single-line comments (starting with `//` and ending with a newline or carriage
return) may be included anywhere spaces would be allowed in the JSON data (that is, not in the middle of a string or 
number, but anywhere between tokens).

### `slashStarComment`

When this option is set to `true`, delimited comments (starting with `/*` and ending with `*/`) may be included anywhere
spaces would be allowed in the JSON data (see [above](#slashslashcomment)).

## Class Diagram

This class diagram may help to explain the main classes and interfaces and the inheritance and interface implementation
relationships between them.

![Class Diagram](diagram.png "UML Class Diagram")

The diagram was produced by [Dia](https://wiki.gnome.org/Apps/Dia/); the diagram file is at [diagram.dia](diagram.dia).

## Dependency Specification

The latest version of the library is 10.2, and it may be obtained from the Maven Central repository.

### Maven
```xml
    <dependency>
      <groupId>io.kjson</groupId>
      <artifactId>kjson-core</artifactId>
      <version>10.2</version>
    </dependency>
```
### Gradle
```groovy
    implementation "io.kjson:kjson-core:10.2"
```
### Gradle (kts)
```kotlin
    implementation("io.kjson:kjson-core:10.2")
```

Peter Wall

2025-06-09
