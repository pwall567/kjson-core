/*
 * @(#) JSONTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021 Peter Wall
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
import io.kjson.JSON.asDecimal
import io.kjson.JSON.asDecimalOrNull
import io.kjson.JSON.asInt
import io.kjson.JSON.asIntOrNull
import io.kjson.JSON.asLong
import io.kjson.JSON.asLongOrNull
import io.kjson.JSON.asObject
import io.kjson.JSON.asObjectOrNull
import io.kjson.JSON.asString
import io.kjson.JSON.asStringOrNull
import io.kjson.JSON.displayValue
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

    @Test fun `should parse using JSON object parseObject`() {
        val json = JSON.parseObject("""{"one":1,"two":2}""")
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
        assertFailsWith<JSONException> { JSON.parseObject("[1,2,3]") }.let {
            expect("Not an object - [...]") { it.message }
        }
    }

    @Test fun `should parse using JSON object parseArray`() {
        val json = JSON.parseArray("""["alpha","beta","gamma"]""")
        expect(3) { json.size }
        expect(JSONString("alpha")) { json[0] }
        expect(JSONString("beta")) { json[1] }
        expect(JSONString("gamma")) { json[2] }
        assertFailsWith<JSONException> { JSON.parseArray("""{"abc":0,"def":-1}""") }.let {
            expect("Not an array - {...}") { it.message }
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

    @Test fun `should return displayValue for array`() {
        expect("[]") { JSONArray.EMPTY.displayValue() }
        expect("[123]") { JSONArray.of(JSONInt(123)).displayValue() }
        expect("[\"Hello\"]") { JSONArray.of(JSONString("Hello")).displayValue() }
        expect("[...]") { JSONArray.of(JSONInt(123), JSONInt(456)).displayValue() }
    }

    @Test fun `should return displayValue for object`() {
        expect("{}") { JSONObject.EMPTY.displayValue() }
        expect("""{"abc":123}""") { JSONObject.of("abc" to JSONInt(123)).displayValue() }
        expect("""{"greeting":"Hello"}""") { JSONObject.of("greeting" to JSONString("Hello")).displayValue() }
        expect("{...}") { JSONObject.of("abc" to JSONInt(123), "def" to JSONInt(456)).displayValue() }
    }

    @Test fun `should return asString for JSONString`() {
        val json = JSONString("boring")
        expect("boring") { json.asString }
        expect("boring") { json.asStringOrNull }
    }

    @Test fun `should fail on attempt to get asString of other types`() {
        val jsonInt = JSONInt(8)
        assertNull(jsonInt.asStringOrNull)
        assertFailsWith<JSONException> { jsonInt.asString }.let {
            expect("Not a string - 8") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertFailsWith<JSONException> { jsonArray.asString }.let {
            expect("Not a string - [\"Testing\"]") { it.message }
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
        assertFailsWith<JSONException> { jsonString.asInt }.let {
            expect("Not an int - \"not a number\"") { it.message }
        }
        val jsonArray = JSONArray.of(JSONString("Testing"))
        assertNull(jsonArray.asIntOrNull)
        assertFailsWith<JSONException> { jsonArray.asInt }.let {
            expect("Not an int - [\"Testing\"]") { it.message }
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
        assertFailsWith<JSONException> { jsonString.asLong }.let {
            expect("Not a long - \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONException> { jsonObject.asLong }.let {
            expect("Not a long - {\"name\":\"value\"}") { it.message }
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
        assertFailsWith<JSONException> { jsonString.asDecimal }.let {
            expect("Not a decimal - \"not a number\"") { it.message }
        }
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertFailsWith<JSONException> { jsonObject.asDecimal }.let {
            expect("Not a decimal - {\"name\":\"value\"}") { it.message }
        }
    }

    @Test fun `should return asBoolean for JSONBoolean`() {
        assertTrue(JSONBoolean.TRUE.asBoolean)
        assertFalse(JSONBoolean.FALSE.asBoolean)
    }

    @Test fun `should fail on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        assertNull(jsonString.asBooleanOrNull)
        assertFailsWith<JSONException> { jsonString.asBoolean }.let {
            expect("Not a boolean - \"not a boolean\"") { it.message }
        }
    }

    @Test fun `should return asArray for JSONArray`() {
        val jsonArray = JSONArray.of(JSONInt(123), JSONInt(456))
        assertSame(jsonArray, jsonArray.asArray)
        assertSame(jsonArray, jsonArray.asArrayOrNull)
    }

    @Test fun `should fail on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        assertNull(jsonString.asArrayOrNull)
        assertFailsWith<JSONException> { jsonString.asArray }.let {
            expect("Not an array - \"not an array\"") { it.message }
        }
    }

    @Test fun `should return asObject for JSONObject`() {
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        assertSame(jsonObject, jsonObject.asObject)
        assertSame(jsonObject, jsonObject.asObjectOrNull)
    }

    @Test fun `should fail on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        assertNull(jsonString.asObjectOrNull)
        assertFailsWith<JSONException> { jsonString.asObject }.let {
            expect("Not an object - \"not an object\"") { it.message }
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
        val sb = StringBuilder()
        formatter.formatTo(sb, json)
        expect(formatted2) { sb.toString() }
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
