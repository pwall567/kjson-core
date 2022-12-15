/*
 * @(#) JSONTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022 Peter Wall
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
import io.kjson.JSON.asArrayOrNull
import io.kjson.JSON.asBoolean
import io.kjson.JSON.asBooleanOrNull
import io.kjson.JSON.asByte
import io.kjson.JSON.asByteOrNull
import io.kjson.JSON.asDecimal
import io.kjson.JSON.asDecimalOrNull
import io.kjson.JSON.asInt
import io.kjson.JSON.asIntOrNull
import io.kjson.JSON.asLong
import io.kjson.JSON.asLongOrNull
import io.kjson.JSON.asObject
import io.kjson.JSON.asObjectOrNull
import io.kjson.JSON.asShort
import io.kjson.JSON.asShortOrNull
import io.kjson.JSON.asString
import io.kjson.JSON.asStringOrNull
import io.kjson.JSON.asUByte
import io.kjson.JSON.asUByteOrNull
import io.kjson.JSON.asUInt
import io.kjson.JSON.asUIntOrNull
import io.kjson.JSON.asULong
import io.kjson.JSON.asULongOrNull
import io.kjson.JSON.asUShort
import io.kjson.JSON.asUShortOrNull
import io.kjson.JSON.displayValue
import io.kjson.JSON.elidedValue
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
        assertFailsWith<JSONIncorrectTypeException> { JSON.parseObject("[1,2,3]") }.let {
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
        assertFailsWith<JSONIncorrectTypeException> { JSON.parseArray("""{"abc":0,"def":-1}""") }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            assertNull(it.key)
            expect("Node not correct type (JSONArray), was { ... }") { it.message }
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
        expect("[123]") { JSONArray.of(JSONInt(123)).displayValue() }
        expect("[\"Hello\"]") { JSONArray.of(JSONString("Hello")).displayValue() }
        expect("[ ... ]") { JSONArray.of(JSONInt(123), JSONInt(456)).displayValue() }
    }

    @Test fun `should return displayValue for object`() {
        expect("{}") { JSONObject.EMPTY.displayValue() }
        expect("""{"abc":123}""") { JSONObject.of("abc" to JSONInt(123)).displayValue() }
        expect("""{"greeting":"Hello"}""") { JSONObject.of("greeting" to JSONString("Hello")).displayValue() }
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

    @Test fun `should return asString for JSONString`() {
        val json = JSONString("boring")
        expect("boring") { json.asString }
        expect("boring") { json.asStringOrNull }
    }

    @Test fun `should fail on attempt to get asString of other types`() {
        val jsonInt = JSONInt(8)
        assertNull(jsonInt.asStringOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonInt.asString }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (String), was 8") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asString }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (String), was [\"Testing\"]") { it.message }
        }
    }

    @Test fun `should return asInt for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asInt }
        expect(8) { jsonInt.asIntOrNull }
        val jsonLong = JSONLong(12345)
        expect(12345) { jsonLong.asInt }
        expect(12345) { jsonLong.asIntOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asInt }
        expect(123) { jsonDecimal.asIntOrNull }
    }

    @Test fun `should fail on attempt to get asInt of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Int), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Int), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asInt }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Int), was 123.5000") { it.message }
        }
    }

    @Test fun `should return asLong for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asLong }
        expect(8) { jsonInt.asLongOrNull }
        val jsonLong = JSONLong(1234567812345678)
        expect(1234567812345678) { jsonLong.asLong }
        expect(1234567812345678) { jsonLong.asLongOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("9876543219876543.0000"))
        expect(9876543219876543) { jsonDecimal.asLong }
        expect(9876543219876543) { jsonDecimal.asLongOrNull }
    }

    @Test fun `should fail on attempt to get asLong of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asLongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Long), was \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONIncorrectTypeException> { jsonObject.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonObject) { it.value }
            expect("Node not correct type (Long), was {\"name\":\"value\"}") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asLongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asLong }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Long), was 123.5000") { it.message }
        }
    }

    @Test fun `should return asShort for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asShort }
        expect(8) { jsonInt.asShortOrNull }
        val jsonLong = JSONLong(12345)
        expect(12345) { jsonLong.asShort }
        expect(12345) { jsonLong.asShortOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asShort }
        expect(123) { jsonDecimal.asShortOrNull }
    }

    @Test fun `should fail on attempt to get asShort of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Short), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Short), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asShort }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Short), was 123.5000") { it.message }
        }
    }

    @Test fun `should return asByte for number types`() {
        val jsonInt = JSONInt(8)
        expect(8) { jsonInt.asByte }
        expect(8) { jsonInt.asByteOrNull }
        val jsonLong = JSONLong(123)
        expect(123) { jsonLong.asByte }
        expect(123) { jsonLong.asByteOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123) { jsonDecimal.asByte }
        expect(123) { jsonDecimal.asByteOrNull }
    }

    @Test fun `should fail on attempt to get asByte of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Byte), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (Byte), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asByte }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (Byte), was 123.5000") { it.message }
        }
    }

    @Test fun `should return asULong for number types`() {
        val jsonInt = JSONInt(8)
        expect(8.toULong()) { jsonInt.asULong }
        expect(8.toULong()) { jsonInt.asULongOrNull }
        val jsonLong = JSONLong(12345)
        expect(12345.toULong()) { jsonLong.asULong }
        expect(12345.toULong()) { jsonLong.asULongOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123.toULong()) { jsonDecimal.asULong }
        expect(123.toULong()) { jsonDecimal.asULongOrNull }
    }

    @Test fun `should fail on attempt to get asULong of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asULongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (ULong), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asULongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (ULong), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asULongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (ULong), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asULongOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonInt.asULong }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (ULong), was -1") { it.message }
        }
    }

    @Test fun `should return asUInt for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUInt }
        expect(8U) { jsonInt.asUIntOrNull }
        val jsonLong = JSONLong(12345)
        expect(12345U) { jsonLong.asUInt }
        expect(12345U) { jsonLong.asUIntOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123U) { jsonDecimal.asUInt }
        expect(123U) { jsonDecimal.asUIntOrNull }
    }

    @Test fun `should fail on attempt to get asUInt of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UInt), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UInt), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UInt), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUIntOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonInt.asUInt }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UInt), was -1") { it.message }
        }
    }

    @Test fun `should return asUShort for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUShort }
        expect(8U) { jsonInt.asUShortOrNull }
        val jsonLong = JSONLong(45678)
        expect(45678U) { jsonLong.asUShort }
        expect(45678U) { jsonLong.asUShortOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("1234.0000"))
        expect(1234U) { jsonDecimal.asUShort }
        expect(1234U) { jsonDecimal.asUShortOrNull }
    }

    @Test fun `should fail on attempt to get asUShort of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UShort), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UShort), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UShort), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUShortOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonInt.asUShort }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UShort), was -1") { it.message }
        }
    }

    @Test fun `should return asUByte for number types`() {
        val jsonInt = JSONInt(8)
        expect(8U) { jsonInt.asUByte }
        expect(8U) { jsonInt.asUByteOrNull }
        val jsonLong = JSONLong(234)
        expect(234U) { jsonLong.asUByte }
        expect(234U) { jsonLong.asUByteOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.0000"))
        expect(123U) { jsonDecimal.asUByte }
        expect(123U) { jsonDecimal.asUByteOrNull }
    }

    @Test fun `should fail on attempt to get asUByte of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asUByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (UByte), was \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asUByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonArray.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonArray) { it.value }
            expect("Node not correct type (UByte), was [\"Testing\"]") { it.message }
        }
        val jsonDecimal = JSONDecimal(BigDecimal("123.5000"))
        assertNull(jsonDecimal.asUByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonDecimal.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonDecimal) { it.value }
            expect("Node not correct type (UByte), was 123.5000") { it.message }
        }
        val jsonInt = JSONInt(-1)
        assertNull(jsonInt.asUByteOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonInt.asUByte }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            assertNull(it.key)
            expect(jsonInt) { it.value }
            expect("Node not correct type (UByte), was -1") { it.message }
        }
    }

    @Test fun `should return asDecimal for number types`() {
        val jsonInt = JSONInt(8)
        expect(BigDecimal(8)) { jsonInt.asDecimal }
        expect(BigDecimal(8)) { jsonInt.asDecimalOrNull }
        val jsonLong = JSONLong(1234567812345678)
        expect(BigDecimal(1234567812345678)) { jsonLong.asDecimal }
        expect(BigDecimal(1234567812345678)) { jsonLong.asDecimalOrNull }
        val jsonDecimal = JSONDecimal(BigDecimal("123.45678"))
        expect(BigDecimal("123.45678")) { jsonDecimal.asDecimal }
        expect(BigDecimal("123.45678")) { jsonDecimal.asDecimalOrNull }
    }

    @Test fun `should fail on attempt to get asDecimal of other types`() {
        val jsonString = JSONString("not a number")
        assertNull(jsonString.asDecimalOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asDecimal }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (BigDecimal), was \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONIncorrectTypeException> { jsonObject.asDecimal }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            assertNull(it.key)
            expect(jsonObject) { it.value }
            expect("Node not correct type (BigDecimal), was {\"name\":\"value\"}") { it.message }
        }
    }

    @Test fun `should return asBoolean for JSONBoolean`() {
        assertTrue(JSONBoolean.TRUE.asBoolean)
        assertTrue(JSONBoolean.TRUE.asBooleanOrNull)
        assertFalse(JSONBoolean.FALSE.asBoolean)
        assertFalse(JSONBoolean.FALSE.asBooleanOrNull)
    }

    @Test fun `should fail on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        assertNull(jsonString.asBooleanOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asBoolean }.let {
            expect("Node") { it.nodeName }
            expect("Boolean") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (Boolean), was \"not a boolean\"") { it.message }
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asArray for JSONArray`() {
        val jsonArray = JSONArray.of(JSONInt(123), JSONInt(456))
        assertSame(jsonArray, jsonArray.asArray)
        assertSame(jsonArray, jsonArray.asArrayOrNull)
    }

    @Test fun `should fail on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        assertNull(jsonString.asArrayOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asArray }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (JSONArray), was \"not an array\"") { it.message }
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asObject for JSONObject`() {
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertSame(jsonObject, jsonObject.asObject)
        assertSame(jsonObject, jsonObject.asObjectOrNull)
    }

    @Test fun `should fail on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        assertNull(jsonString.asObjectOrNull)
        assertFailsWith<JSONIncorrectTypeException> { jsonString.asObject }.let {
            expect("Node") { it.nodeName }
            expect("JSONObject") { it.target }
            assertNull(it.key)
            expect(jsonString) { it.value }
            expect("Node not correct type (JSONObject), was \"not an object\"") { it.message }
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
