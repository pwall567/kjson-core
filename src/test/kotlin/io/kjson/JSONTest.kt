/*
 * @(#) JSONTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023 Peter Wall
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package io.kjson

import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

import io.kjson.JSON.asArray
import io.kjson.JSON.asArrayOrError
import io.kjson.JSON.asArrayOrNull
import io.kjson.JSON.asBoolean
import io.kjson.JSON.asBooleanOrError
import io.kjson.JSON.asBooleanOrNull
import io.kjson.JSON.asByte
import io.kjson.JSON.asByteOrError
import io.kjson.JSON.asByteOrNull
import io.kjson.JSON.asDecimal
import io.kjson.JSON.asDecimalOrError
import io.kjson.JSON.asDecimalOrNull
import io.kjson.JSON.asInt
import io.kjson.JSON.asIntOrError
import io.kjson.JSON.asIntOrNull
import io.kjson.JSON.asLong
import io.kjson.JSON.asLongOrError
import io.kjson.JSON.asLongOrNull
import io.kjson.JSON.asObject
import io.kjson.JSON.asObjectOrError
import io.kjson.JSON.asObjectOrNull
import io.kjson.JSON.asShort
import io.kjson.JSON.asShortOrError
import io.kjson.JSON.asShortOrNull
import io.kjson.JSON.asString
import io.kjson.JSON.asStringOrError
import io.kjson.JSON.asStringOrNull
import io.kjson.JSON.asUByte
import io.kjson.JSON.asUByteOrError
import io.kjson.JSON.asUByteOrNull
import io.kjson.JSON.asUInt
import io.kjson.JSON.asUIntOrError
import io.kjson.JSON.asUIntOrNull
import io.kjson.JSON.asULong
import io.kjson.JSON.asULongOrError
import io.kjson.JSON.asULongOrNull
import io.kjson.JSON.asUShort
import io.kjson.JSON.asUShortOrError
import io.kjson.JSON.asUShortOrNull
import io.kjson.JSON.displayValue
import io.kjson.JSON.elidedValue
import io.kjson.JSON.typeError
import net.pwall.json.format.Formatter
import net.pwall.json.format.Formatter.unixLineSeparator

class JSONTest {

    @Test fun `should create values using JSON object`() {
        val testInt = JSON.of(54321)
        expect(JSONInt(54321)) { testInt }
        val testLong = JSON.of(2233445566778899)
        expect(JSONLong(2233445566778899)) { testLong }
        val testDecimal = JSON.of(BigDecimal("99.999"))
        expect(JSONDecimal(BigDecimal("99.999"))) { testDecimal }
        val testString = JSON.of("Hello!")
        expect(JSONString("Hello!")) { testString }
    }

    @Test fun `should parse using JSON object`() {
        val json = JSON.parse("""{"one":1,"two":2}""")
        assertTrue(json is JSONObject)
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
    }

    @Test fun `should parse and test for null using JSON object`() {
        val json = JSON.parseNonNull("""{"one":1,"two":2}""")
        assertTrue(json is JSONObject)
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
    }

    @Test fun `should fail when parsing non null using JSON object`() {
        assertFailsWith<JSONException> { JSON.parseNonNull("null") }.let {
            expect("JSON must not be \"null\"") { it.message }
        }
    }

    @Test fun `should parse using JSON object parseObject`() {
        val json = JSON.parseObject("""{"one":1,"two":2}""")
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
        assertFailsWith<JSONTypeException> { JSON.parseObject("[1,2,3]") }.let {
            expect("Node") { it.nodeName }
            expect("JSONObject") { it.target }
            assertNull(it.key)
            expect("Node not correct type (JSONObject), was [ ... ]") { it.message }
        }
    }

    @Test fun `should parse using JSON object parseArray`() {
        val json = JSON.parseArray("""["alpha","beta","gamma"]""")
        expect(3) { json.size }
        expect(JSONString("alpha")) { json[0] }
        expect(JSONString("beta")) { json[1] }
        expect(JSONString("gamma")) { json[2] }
        assertFailsWith<JSONTypeException> { JSON.parseArray("""{"abc":0,"def":-1}""") }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            assertNull(it.key)
            expect("Node not correct type (JSONArray), was { ... }") { it.message }
        }
    }

    @Test fun `should parse JSON Lines`() {
        val json = JSON.parseLines("{\"aa\":123,\"bb\":321}\n{\"aa\":777,\"bb\":888}")
        expect(2) { json.size }
        with(json[0]) {
            assertTrue(this is JSONObject)
            expect(2) { size }
            expect(123) { this["aa"].asInt }
            expect(321) { this["bb"].asInt }
        }
        with(json[1]) {
            assertTrue(this is JSONObject)
            expect(2) { size }
            expect(777) { this["aa"].asInt }
            expect(888) { this["bb"].asInt }
        }
    }

    @Test fun `should return displayValue for number types`() {
        expect("0") { JSONInt(0).displayValue() }
        expect("12345") { JSONInt(12345).displayValue() }
        expect("1234567812345678") { JSONLong(1234567812345678).displayValue() }
        expect("0.123") { JSONDecimal(BigDecimal("0.123")).displayValue() }
    }

    @Test fun `should return displayValue for boolean`() {
        expect("true") { JSONBoolean.TRUE.displayValue() }
        expect("false") { JSONBoolean.FALSE.displayValue() }
    }

    @Test fun `should return displayValue for null`() {
        expect("null") { JSON.parse("null").displayValue() }
    }

    @Test fun `should return displayValue for string`() {
        expect("\"abc\"") { JSONString("abc").displayValue() }
        expect("\"the quic ... lazy dog\"") { JSONString("the quick brown fox jumps over the lazy dog").displayValue() }
    }

    @Test fun `should return displayValue for string with specified maximum`() {
        expect("\"abc\"") { JSONString("abc").displayValue(17) }
        expect("\"the qu ... zy dog\"") { JSONString("the quick brown fox jumps over the lazy dog").displayValue(17) }
    }

    @Test fun `should return displayValue for array`() {
        expect("[]") { JSONArray.EMPTY.displayValue() }
        expect("[ ... ]") { JSONArray.of(JSONString("Hello")).displayValue() }
        expect("[ ... ]") { JSONArray.of(JSONInt(123), JSONInt(456)).displayValue() }
    }

    @Test fun `should return displayValue for object`() {
        expect("{}") { JSONObject.EMPTY.displayValue() }
        expect("{ ... }") { JSONObject.of("abc" to JSONInt(123)).displayValue() }
        expect("{ ... }") { JSONObject.of("abc" to JSONInt(123), "def" to JSONInt(456)).displayValue() }
    }

    @Test fun `should return elidedValue for number types`() {
        expect("0") { JSONInt(0).elidedValue() }
        expect("12345") { JSONInt(12345).elidedValue() }
        expect("1234567812345678") { JSONLong(1234567812345678).elidedValue() }
        expect("0.123") { JSONDecimal(BigDecimal("0.123")).elidedValue() }
    }

    @Test fun `should return elidedValue for boolean`() {
        expect("true") { JSONBoolean.TRUE.elidedValue() }
        expect("false") { JSONBoolean.FALSE.elidedValue() }
    }

    @Test fun `should return elidedValue for null`() {
        expect("null") { JSON.parse("null").elidedValue() }
    }

    @Test fun `should return elidedValue for string`() {
        expect("\"abc\"") { JSONString("abc").elidedValue() }
    }

    @Test fun `should return elidedValue for array`() {
        expect("[]") { JSONArray.EMPTY.elidedValue() }
        expect("[123]") { JSONArray.of(JSONInt(123)).elidedValue() }
        expect("[\"Hello\"]") { JSONArray.of(JSONString("Hello")).elidedValue() }
        expect("[123,456]") { JSONArray.of(JSONInt(123), JSONInt(456)).elidedValue() }
    }

    @Test fun `should return elidedValue for object with no exclusions or inclusions`() {
        expect("{}") { JSONObject.EMPTY.elidedValue() }
        expect("""{"abc":123}""") { JSONObject.of("abc" to JSONInt(123)).elidedValue() }
        expect("""{"greeting":"Hello"}""") { JSONObject.of("greeting" to JSONString("Hello")).elidedValue() }
        expect("""{"abc":123,"def":4}""") { JSONObject.of("abc" to JSONInt(123), "def" to JSONInt(4)).elidedValue() }
    }

    @Test fun `should return elidedValue for object with exclusions`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":444,"eee":555}""") { json.elidedValue() }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":444,"eee":555}""") { json.elidedValue(exclude = emptyList()) }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":"****","eee":555}""") {
            json.elidedValue(exclude = setOf("ddd"))
        }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":"****","eee":"****"}""") {
            json.elidedValue(exclude = setOf("ddd", "eee"))
        }
    }

    @Test fun `should return elidedValue for object with inclusions`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        expect("""{"aaa":"****","bbb":"****","ccc":"****","ddd":"****","eee":"****"}""") {
            json.elidedValue(include = emptyList())
        }
        expect("""{"aaa":"****","bbb":"****","ccc":"****","ddd":444,"eee":"****"}""") {
            json.elidedValue(include = setOf("ddd"))
        }
        expect("""{"aaa":"****","bbb":"****","ccc":"****","ddd":444,"eee":555}""") {
            json.elidedValue(include = setOf("ddd", "eee"))
        }
    }

    @Test fun `should return elidedValue for object with custom substitute string`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":"","eee":555}""") {
            json.elidedValue(exclude = setOf("ddd"), substitute = "")
        }
        expect("""{"aaa":111,"bbb":222,"ccc":333,"ddd":"elided\u2020","eee":"elided\u2020"}""") {
            json.elidedValue(exclude = setOf("ddd", "eee"), substitute = "elided\u2020")
        }
    }

    @Test fun `should return elidedValue for array of object with no exclusions or inclusions`() {
        val obj1 = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
        }
        val obj2 = JSONObject.build {
            add("ddd", 444)
            add("eee", 555)
        }
        expect("""[{"aaa":111,"bbb":222,"ccc":333},{"ddd":444,"eee":555}]""") { JSONArray.of(obj1, obj2).elidedValue() }
    }

    @Test fun `should return elidedValue for array of object with exclusions`() {
        val obj1 = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
        }
        val obj2 = JSONObject.build {
            add("ddd", 444)
            add("eee", 555)
        }
        expect("""[{"aaa":111,"bbb":222,"ccc":"****"},{"ddd":444,"eee":555}]""") {
            JSONArray.of(obj1, obj2).elidedValue(exclude = setOf("ccc"))
        }
    }

    @Test fun `should return asString for JSONString`() {
        val json = JSONString("boring")
        expect("boring") { json.asString }
        expect("boring") { json.asStringOrNull }
        expect("boring") { json.asStringOrError("string", 333, "Item") }
    }

    @Test fun `should fail on attempt to get asString of other types`() {
        val jsonInt = JSONInt(8)
        assertNull(jsonInt.asStringOrNull)
        assertFailsWith<JSONTypeException> { jsonInt.asString }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (String), was 8") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertFailsWith<JSONTypeException> { jsonArray.asString }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (String), was [ ... ]") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asStringOrError of other types`() {
        val jsonInt = JSONInt(8)
        assertFailsWith<JSONTypeException> {
            jsonInt.asStringOrError("string", "test1", "Value")
        }.let {
            expect("Value") { it.nodeName }
            expect("string") { it.target }
            expect("test1") { it.key }
            expect(jsonInt) { it.value }
            expect("Value not correct type (string), was 8, at test1") { it.message }
        }
    }

    @Test fun `should return asInt for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asInt }
        expect(8) { jsonInt.asIntOrNull }
        expect(8) { jsonInt.asIntOrError("xxx", "yyy", "zzz") }
        val jsonLong = JSONLong(12345)
        expect(12345) { jsonLong.asInt }
        expect(12345) { jsonLong.asIntOrNull }
        expect(12345) { jsonLong.asIntOrError(key = 27) }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asInt }
        expect(123) { jsonDecimal.asIntOrNull }
        expect(123) { jsonDecimal.asIntOrError("aaa") }
    }

    @Test fun `should fail on attempt to get asInt of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asIntOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Int), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asIntOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Int), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asIntOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Int), was 123.5000") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asIntOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asIntOrError("integer", "test", "Value")
        }.let {
            expect("Value") { it.nodeName }
            expect("integer") { it.target }
            expect("test") { it.key }
            expect(jsonString) { it.value }
            expect("Value not correct type (integer), was \"not a number\", at test") { it.message }
        }
    }

    @Test fun `should return asLong for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asLong }
        expect(8) { jsonInt.asLongOrNull }
        val jsonLong = JSONLong(1234567812345678)
        expect(1234567812345678) { jsonLong.asLong }
        expect(1234567812345678) { jsonLong.asLongOrNull }
        expect(1234567812345678) { jsonLong.asLongOrError("long integer", 999) }
        val jsonDecimal = JSONDecimal(BigDecimal("9876543219876543.0000"))
        expect(9876543219876543) { jsonDecimal.asLong }
        expect(9876543219876543) { jsonDecimal.asLongOrNull }
        expect(9876543219876543) { jsonDecimal.asLongOrError(key = 6, nodeName = "Property") }
    }

    @Test fun `should fail on attempt to get asLong of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asLongOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Long), was \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONTypeException> { jsonObject.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonObject) { it.value }
            expect("Node not correct type (Long), was { ... }") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asLongOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Long), was 123.5000") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asLongOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asLongOrError("long", "test", "Value")
        }.let {
            expect("Value") { it.nodeName }
            expect("long") { it.target }
            expect("test") { it.key }
            expect(jsonString) { it.value }
            expect("Value not correct type (long), was \"not a number\", at test") { it.message }
        }
    }

    @Test fun `should return asShort for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asShort }
        expect(8) { jsonInt.asShortOrNull }
        expect(8) { jsonInt.asShortOrError("short") }
        val jsonLong = JSONLong(12345)
        expect(12345) { jsonLong.asShort }
        expect(12345) { jsonLong.asShortOrNull }
        expect(12345) { jsonLong.asShortOrError(key = "key") }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asShort }
        expect(123) { jsonDecimal.asShortOrNull }
        expect(123) { jsonDecimal.asShortOrError("short", 1) }
    }

    @Test fun `should fail on attempt to get asShort of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asShortOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Short), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asShortOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Short), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asShortOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Short), was 123.5000") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asShortOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asShortOrError("short", "test99", "Value")
        }.let {
            expect("Value") { it.nodeName }
            expect("short") { it.target }
            expect("test99") { it.key }
            expect(jsonString) { it.value }
            expect("Value not correct type (short), was \"not a number\", at test99") { it.message }
        }
    }

    @Test fun `should return asByte for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asByte }
        expect(8) { jsonInt.asByteOrNull }
        expect(8) { jsonInt.asByteOrError() }
        val jsonLong = JSONLong(123)
        expect(123) { jsonLong.asByte }
        expect(123) { jsonLong.asByteOrNull }
        expect(123) { jsonLong.asByteOrError("b", "c", "d") }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asByte }
        expect(123) { jsonDecimal.asByteOrNull }
        expect(123) { jsonDecimal.asByteOrError("byte", "test4") }
    }

    @Test fun `should fail on attempt to get asByte of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asByteOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Byte), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asByteOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Byte), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asByteOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Byte), was 123.5000") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asByteOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asByteOrError("byte", "test123", "Item")
        }.let {
            expect("Item") { it.nodeName }
            expect("byte") { it.target }
            expect("test123") { it.key }
            expect(jsonString) { it.value }
            expect("Item not correct type (byte), was \"not a number\", at test123") { it.message }
        }
    }

    @Test fun `should return asULong for number types`() {
        val jsonInt = JSONInt(8)
        expect(8.toULong()) { jsonInt.asULong }
        expect(8.toULong()) { jsonInt.asULongOrNull }
        expect(8.toULong()) { jsonInt.asULongOrError() }
        val jsonLong = JSONLong(12345)
        expect(12345.toULong()) { jsonLong.asULong }
        expect(12345.toULong()) { jsonLong.asULongOrNull }
        expect(12345.toULong()) { jsonLong.asULongOrError(key = 27) }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123.toULong()) { jsonDecimal.asULong }
        expect(123.toULong()) { jsonDecimal.asULongOrNull }
        expect(123.toULong()) { jsonDecimal.asULongOrError("whatever") }
    }

    @Test fun `should fail on attempt to get asULong of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asULongOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (ULong), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asULongOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (ULong), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asULongOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (ULong), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asULongOrNull)
        assertFailsWith<JSONTypeException> { jsonInt.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (ULong), was -1") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asULongOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asULongOrError("unsigned long", "test2", "Item")
        }.let {
            expect("Item") { it.nodeName }
            expect("unsigned long") { it.target }
            expect("test2") { it.key }
            expect(jsonString) { it.value }
            expect("Item not correct type (unsigned long), was \"not a number\", at test2") { it.message }
        }
    }

    @Test fun `should return asUInt for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUInt }
        expect(8U) { jsonInt.asUIntOrNull }
        expect(8U) { jsonInt.asUIntOrError("stuff", 15, "Thing") }
        val jsonLong = JSONLong(12345)
        expect(12345U) { jsonLong.asUInt }
        expect(12345U) { jsonLong.asUIntOrNull }
        expect(12345U) { jsonLong.asUIntOrError() }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123U) { jsonDecimal.asUInt }
        expect(123U) { jsonDecimal.asUIntOrNull }
        expect(123U) { jsonDecimal.asUIntOrError("unsigned int", "property", "Property") }
    }

    @Test fun `should fail on attempt to get asUInt of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUIntOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UInt), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUIntOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UInt), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUIntOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UInt), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUIntOrNull)
        assertFailsWith<JSONTypeException> { jsonInt.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UInt), was -1") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asUIntOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asUIntOrError("unsigned int", "test1234", "Item")
        }.let {
            expect("Item") { it.nodeName }
            expect("unsigned int") { it.target }
            expect("test1234") { it.key }
            expect(jsonString) { it.value }
            expect("Item not correct type (unsigned int), was \"not a number\", at test1234") { it.message }
        }
    }

    @Test fun `should return asUShort for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUShort }
        expect(8U) { jsonInt.asUShortOrNull }
        expect(8U) { jsonInt.asUShortOrError("a", "b", "c") }
        val jsonLong = JSONLong(45678)
        expect(45678U) { jsonLong.asUShort }
        expect(45678U) { jsonLong.asUShortOrNull }
        expect(45678U) { jsonLong.asUShortOrError() }
        val jsonDecimal = JSONDecimal(BigDecimal("1234.0000"))
        expect(1234U) { jsonDecimal.asUShort }
        expect(1234U) { jsonDecimal.asUShortOrNull }
        expect(1234U) { jsonDecimal.asUShortOrError(key = "it") }
    }

    @Test fun `should fail on attempt to get asUShort of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUShortOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UShort), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUShortOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UShort), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUShortOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UShort), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUShortOrNull)
        assertFailsWith<JSONTypeException> { jsonInt.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UShort), was -1") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asUShortOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asUShortOrError("unsigned short", "testtest", "Item")
        }.let {
            expect("Item") { it.nodeName }
            expect("unsigned short") { it.target }
            expect("testtest") { it.key }
            expect(jsonString) { it.value }
            expect("Item not correct type (unsigned short), was \"not a number\", at testtest") { it.message }
        }
    }

    @Test fun `should return asUByte for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUByte }
        expect(8U) { jsonInt.asUByteOrNull }
        expect(8U) { jsonInt.asUByteOrError("unsigned byte", nodeName = "Item") }
        val jsonLong = JSONLong(234)
        expect(234U) { jsonLong.asUByte }
        expect(234U) { jsonLong.asUByteOrNull }
        expect(234U) { jsonLong.asUByteOrError() }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123U) { jsonDecimal.asUByte }
        expect(123U) { jsonDecimal.asUByteOrNull }
        expect(123U) { jsonDecimal.asUByteOrError("type", "key", "name") }
    }

    @Test fun `should fail on attempt to get asUByte of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUByteOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UByte), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUByteOrNull)
        assertFailsWith<JSONTypeException> { jsonArray.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UByte), was [ ... ]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUByteOrNull)
        assertFailsWith<JSONTypeException> { jsonDecimal.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UByte), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUByteOrNull)
        assertFailsWith<JSONTypeException> { jsonInt.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UByte), was -1") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asUByteOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asUByteOrError("unsigned byte", "test333")
        }.let {
            expect("Node") { it.nodeName }
            expect("unsigned byte") { it.target }
            expect("test333") { it.key }
            expect(jsonString) { it.value }
            expect("Node not correct type (unsigned byte), was \"not a number\", at test333") { it.message }
        }
    }

    @Test fun `should return asDecimal for number types`() {
        val jsonInt = JSONInt(8)
        expect(BigDecimal(8)) { jsonInt.asDecimal }
        expect(BigDecimal(8)) { jsonInt.asDecimalOrNull }
        expect(BigDecimal(8)) { jsonInt.asDecimalOrError("a", "b", "c") }
        val jsonLong = JSONLong(1234567812345678)
        expect(BigDecimal(1234567812345678)) { jsonLong.asDecimal }
        expect(BigDecimal(1234567812345678)) { jsonLong.asDecimalOrNull }
        expect(BigDecimal(1234567812345678)) { jsonLong.asDecimalOrError(key = 9) }
        val jsonDecimal = JSONDecimal(BigDecimal("123.45678"))
        expect(BigDecimal("123.45678")) { jsonDecimal.asDecimal }
        expect(BigDecimal("123.45678")) { jsonDecimal.asDecimalOrNull }
        expect(BigDecimal("123.45678")) { jsonDecimal.asDecimalOrError() }
    }

    @Test fun `should fail on attempt to get asDecimal of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asDecimalOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asDecimal }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (BigDecimal), was \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONTypeException> { jsonObject.asDecimal }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            assertNull(it.key)
            expect(jsonObject) { it.value }
            expect("Node not correct type (BigDecimal), was { ... }") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asDecimalOrError of other types`() {
        val jsonString = JSONString("not a number")
        assertFailsWith<JSONTypeException> {
            jsonString.asDecimalOrError("decimal number", 1000, "Property")
        }.let {
            expect("Property") { it.nodeName }
            expect("decimal number") { it.target }
            expect(1000) { it.key }
            expect(jsonString) { it.value }
            expect("Property not correct type (decimal number), was \"not a number\", at 1000") { it.message }
        }
    }

    @Test fun `should return asBoolean for JSONBoolean`() {
        assertTrue(JSONBoolean.TRUE.asBoolean)
        assertTrue(JSONBoolean.TRUE.asBooleanOrNull)
        assertTrue(JSONBoolean.TRUE.asBooleanOrError("boolean"))
        assertFalse(JSONBoolean.FALSE.asBoolean)
        assertFalse(JSONBoolean.FALSE.asBooleanOrNull)
        assertFalse(JSONBoolean.FALSE.asBooleanOrError(key = 0))
    }

    @Test fun `should fail on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        assertNull(jsonString.asBooleanOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asBoolean }.let {
            expect("Node") { it.nodeName }
            expect("Boolean") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Boolean), was \"not a boolean\"") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        assertFailsWith<JSONTypeException> {
            jsonString.asBooleanOrError("boolean", "aaa", "Flag")
        }.let {
            expect("Flag") { it.nodeName }
            expect("boolean") { it.target }
            expect("aaa") { it.key }
            expect(jsonString) { it.value }
            expect("Flag not correct type (boolean), was \"not a boolean\", at aaa") { it.message }
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asArray for JSONArray`() {
        val jsonArray = JSONArray.of(JSONInt(123), JSONInt(456))
        assertSame(jsonArray, jsonArray.asArray)
        assertSame(jsonArray, jsonArray.asArrayOrNull)
        assertSame(jsonArray, jsonArray.asArrayOrError("array"))
    }

    @Test fun `should fail on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        assertNull(jsonString.asArrayOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asArray }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (JSONArray), was \"not an array\"") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        assertFailsWith<JSONTypeException> {
            jsonString.asArrayOrError("array")
        }.let {
            expect("Node") { it.nodeName }
            expect("array") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (array), was \"not an array\"") { it.message }
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asObject for JSONObject`() {
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertSame(jsonObject, jsonObject.asObject)
        assertSame(jsonObject, jsonObject.asObjectOrNull)
        assertSame(jsonObject, jsonObject.asObjectOrError("object"))
    }

    @Test fun `should fail on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        assertNull(jsonString.asObjectOrNull)
        assertFailsWith<JSONTypeException> { jsonString.asObject }.let {
            expect("Node") { it.nodeName }
            expect("JSONObject") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (JSONObject), was \"not an object\"") { it.message }
        }
    }

    @Test fun `should throw custom error on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        assertFailsWith<JSONTypeException> {
            jsonString.asObjectOrError("object", nodeName = "Array")
        }.let {
            expect("Array") { it.nodeName }
            expect("object") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Array not correct type (object), was \"not an object\"") { it.message }
        }
    }

    @Test fun `should throw general type error`() {
        val json = JSONString("this is a string")
        assertFailsWith<JSONTypeException> { json.typeError("integer") }.let {
            expect("Node") { it.nodeName }
            expect("integer") { it.target }
            assertNull(it.key)
            expect(json) { it.value }
            expect("Node not correct type (integer), was \"this is a string\"") { it.message }
        }
    }

    @Test fun `should produce structures capable of being formatted by json-simple - 1`() {
        val input = """{"aaa":1,"bbb":true,"ccc":"\u2014","ddd":1.0,"eee":{},"fff":[],"ggg":null}"""
        val json = JSON.parse(input)
        val formatter = Formatter(unixLineSeparator)
        val sb = StringBuilder()
        formatter.formatTo(sb, json)
        expect(formatted1) { sb.toString() }
        expect(input) { Formatter.output(json) }
    }

    @Test fun `should produce structures capable of being formatted by json-simple - 2`() {
        val input = """{"aaa":{"bbb":{"ccc":{"ddd":{},"eee":[1,2,3,4,5]}}}}"""
        val json = JSON.parse(input)
        val formatter = Formatter(unixLineSeparator)
        val formatted = buildString { formatter.formatTo(this, json) }
        expect(formatted2) { formatted }
        expect(input) { Formatter.output(json) }
    }

    companion object {

        const val formatted1 = """{
  "aaa": 1,
  "bbb": true,
  "ccc": "\u2014",
  "ddd": 1.0,
  "eee": {},
  "fff": [],
  "ggg": null
}"""

        const val formatted2 = """{
  "aaa": {
    "bbb": {
      "ccc": {
        "ddd": {},
        "eee": [
          1,
          2,
          3,
          4,
          5
        ]
      }
    }
  }
}"""

    }

}
