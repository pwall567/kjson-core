/*
 * @(#) JSONTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024 Peter Wall
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

import java.math.BigDecimal

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeSameInstance
import io.kstuff.test.shouldBeType
import io.kstuff.test.shouldThrow

import io.jstuff.json.format.Formatter
import io.jstuff.json.format.Formatter.unixLineSeparator

import io.kjson.JSON.asArray
import io.kjson.JSON.asArrayOr
import io.kjson.JSON.asArrayOrError
import io.kjson.JSON.asArrayOrNull
import io.kjson.JSON.asBoolean
import io.kjson.JSON.asBooleanOr
import io.kjson.JSON.asBooleanOrError
import io.kjson.JSON.asBooleanOrNull
import io.kjson.JSON.asByte
import io.kjson.JSON.asByteOr
import io.kjson.JSON.asByteOrError
import io.kjson.JSON.asByteOrNull
import io.kjson.JSON.asDecimal
import io.kjson.JSON.asDecimalOr
import io.kjson.JSON.asDecimalOrError
import io.kjson.JSON.asDecimalOrNull
import io.kjson.JSON.asInt
import io.kjson.JSON.asIntOr
import io.kjson.JSON.asIntOrError
import io.kjson.JSON.asIntOrNull
import io.kjson.JSON.asLong
import io.kjson.JSON.asLongOr
import io.kjson.JSON.asLongOrError
import io.kjson.JSON.asLongOrNull
import io.kjson.JSON.asNumber
import io.kjson.JSON.asNumberOr
import io.kjson.JSON.asNumberOrError
import io.kjson.JSON.asNumberOrNull
import io.kjson.JSON.asObject
import io.kjson.JSON.asObjectOr
import io.kjson.JSON.asObjectOrError
import io.kjson.JSON.asObjectOrNull
import io.kjson.JSON.asShort
import io.kjson.JSON.asShortOr
import io.kjson.JSON.asShortOrError
import io.kjson.JSON.asShortOrNull
import io.kjson.JSON.asString
import io.kjson.JSON.asStringOr
import io.kjson.JSON.asStringOrError
import io.kjson.JSON.asStringOrNull
import io.kjson.JSON.asUByte
import io.kjson.JSON.asUByteOr
import io.kjson.JSON.asUByteOrError
import io.kjson.JSON.asUByteOrNull
import io.kjson.JSON.asUInt
import io.kjson.JSON.asUIntOr
import io.kjson.JSON.asUIntOrError
import io.kjson.JSON.asUIntOrNull
import io.kjson.JSON.asULong
import io.kjson.JSON.asULongOr
import io.kjson.JSON.asULongOrError
import io.kjson.JSON.asULongOrNull
import io.kjson.JSON.asUShort
import io.kjson.JSON.asUShortOr
import io.kjson.JSON.asUShortOrError
import io.kjson.JSON.asUShortOrNull
import io.kjson.JSON.displayValue
import io.kjson.JSON.elidedValue
import io.kjson.JSON.parseJSONArray
import io.kjson.JSON.parseJSONObject
import io.kjson.JSON.parseJSONValue
import io.kjson.JSON.typeError

class JSONTest {

    @Test fun `should create values using JSON object`() {
        val testInt = JSON.of(54321)
        testInt shouldBe JSONInt(54321)
        val testLong = JSON.of(2233445566778899)
        testLong shouldBe JSONLong(2233445566778899)
        val testDecimal = JSON.of("99.999".toBigDecimal())
        testDecimal shouldBe JSONDecimal("99.999".toBigDecimal())
        val testString = JSON.of("Hello!")
        testString shouldBe JSONString("Hello!")
    }

    @Test fun `should parse using JSON object`() {
        val json = JSON.parse("""{"one":1,"two":2}""")
        json.shouldBeType<JSONObject>()
        json.size shouldBe 2
        json["one"] shouldBe JSONInt(1)
        json["two"] shouldBe JSONInt(2)
    }

    @Test fun `should parse and test for null using JSON object`() {
        val json = JSON.parseNonNull("""{"one":1,"two":2}""")
        json.shouldBeType<JSONObject>()
        json.size shouldBe 2
        json["one"] shouldBe JSONInt(1)
        json["two"] shouldBe JSONInt(2)
    }

    @Test fun `should fail when parsing non null using JSON object`() {
        shouldThrow<JSONException>("JSON must not be \"null\"") {
            JSON.parseNonNull("null")
        }
    }

    @Test fun `should parse using JSON object parseObject`() {
        val json = JSON.parseObject("""{"one":1,"two":2}""")
        json.size shouldBe 2
        json["one"] shouldBe JSONInt(1)
        json["two"] shouldBe JSONInt(2)
        shouldThrow<JSONTypeException>("Node not correct type (JSONObject), was [ ... ]") {
            JSON.parseObject("[1,2,3]")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONObject"
            it.key shouldBe null
        }
    }

    @Test fun `should parse using JSON object parseArray`() {
        val json = JSON.parseArray("""["alpha","beta","gamma"]""")
        json.size shouldBe 3
        json[0] shouldBe JSONString("alpha")
        json[1] shouldBe JSONString("beta")
        json[2] shouldBe JSONString("gamma")
        shouldThrow<JSONTypeException>("Node not correct type (JSONArray), was { ... }") {
            JSON.parseArray("""{"abc":0,"def":-1}""")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONArray"
            it.key shouldBe null
        }
    }

    @Test fun `should parse JSON Lines`() {
        val json = JSON.parseLines("{\"aa\":123,\"bb\":321}\n{\"aa\":777,\"bb\":888}")
        json.size shouldBe 2
        with(json[0]) {
            shouldBeType<JSONObject>()
            size shouldBe 2
            this["aa"].asInt shouldBe 123
            this["bb"].asInt shouldBe 321
        }
        with(json[1]) {
            shouldBeType<JSONObject>()
            size shouldBe 2
            this["aa"].asInt shouldBe 777
            this["bb"].asInt shouldBe 888
        }
    }

    @Test fun `should parse JSONValue using extension function`() {
        val json = """{"one":1,"two":2}""".parseJSONValue()
        json.shouldBeType<JSONObject>()
        json.size shouldBe 2
        json["one"] shouldBe JSONInt(1)
        json["two"] shouldBe JSONInt(2)
    }

    @Test fun `should parse JSONArray using extension function`() {
        val json = """["alpha","beta","gamma"]""".parseJSONArray()
        json.size shouldBe 3
        json[0] shouldBe JSONString("alpha")
        json[1] shouldBe JSONString("beta")
        json[2] shouldBe JSONString("gamma")
    }

    @Test fun `should parse JSONObject using extension function`() {
        val json = """{"one":1,"two":2}""".parseJSONObject()
        json.size shouldBe 2
        json["one"] shouldBe JSONInt(1)
        json["two"] shouldBe JSONInt(2)
    }

    @Test fun `should return displayValue for number types`() {
        JSONInt(0).displayValue() shouldBe "0"
        JSONInt(12345).displayValue() shouldBe "12345"
        JSONLong(1234567812345678).displayValue() shouldBe "1234567812345678"
        JSONDecimal("0.123".toBigDecimal()).displayValue() shouldBe "0.123"
    }

    @Test fun `should return displayValue for boolean`() {
        JSONBoolean.TRUE.displayValue() shouldBe "true"
        JSONBoolean.FALSE.displayValue() shouldBe "false"
    }

    @Test fun `should return displayValue for null`() {
        JSON.parse("null").displayValue() shouldBe "null"
    }

    @Test fun `should return displayValue for string`() {
        JSONString("abc").displayValue() shouldBe "\"abc\""
        JSONString("the quick brown fox jumps over the lazy dog").displayValue() shouldBe "\"the quic ... lazy dog\""
    }

    @Test fun `should return displayValue for string with specified maximum`() {
        JSONString("abc").displayValue(17) shouldBe "\"abc\""
        JSONString("the quick brown fox jumps over the lazy dog").displayValue(17) shouldBe "\"the qu ... zy dog\""
    }

    @Test fun `should return displayValue for array`() {
        JSONArray.EMPTY.displayValue() shouldBe "[]"
        JSONArray.of(JSONString("Hello")).displayValue() shouldBe "[ ... ]"
        JSONArray.of(JSONInt(123), JSONInt(456)).displayValue() shouldBe "[ ... ]"
    }

    @Test fun `should return displayValue for object`() {
        JSONObject.EMPTY.displayValue() shouldBe "{}"
        JSONObject.of("abc" to JSONInt(123)).displayValue() shouldBe "{ ... }"
        JSONObject.of("abc" to JSONInt(123), "def" to JSONInt(456)).displayValue() shouldBe "{ ... }"
    }

    @Test fun `should return elidedValue for number types`() {
        JSONInt(0).elidedValue() shouldBe "0"
        JSONInt(12345).elidedValue() shouldBe "12345"
        JSONLong(1234567812345678).elidedValue() shouldBe "1234567812345678"
        JSONDecimal("0.123".toBigDecimal()).elidedValue() shouldBe "0.123"
    }

    @Test fun `should return elidedValue for boolean`() {
        JSONBoolean.TRUE.elidedValue() shouldBe "true"
        JSONBoolean.FALSE.elidedValue() shouldBe "false"
    }

    @Test fun `should return elidedValue for null`() {
        JSON.parse("null").elidedValue() shouldBe "null"
    }

    @Test fun `should return elidedValue for string`() {
        JSONString("abc").elidedValue() shouldBe "\"abc\""
    }

    @Test fun `should return elidedValue for array`() {
        JSONArray.EMPTY.elidedValue() shouldBe "[]"
        JSONArray.of(JSONInt(123)).elidedValue() shouldBe "[123]"
        JSONArray.of(JSONString("Hello")).elidedValue() shouldBe "[\"Hello\"]"
        JSONArray.of(JSONInt(123), JSONInt(456)).elidedValue() shouldBe "[123,456]"
    }

    @Test fun `should return elidedValue for object with no exclusions or inclusions`() {
        JSONObject.EMPTY.elidedValue() shouldBe "{}"
        JSONObject.of("abc" to JSONInt(123)).elidedValue() shouldBe """{"abc":123}"""
        JSONObject.of("greeting" to JSONString("Hello")).elidedValue() shouldBe """{"greeting":"Hello"}"""
        JSONObject.of("abc" to JSONInt(123), "def" to JSONInt(4)).elidedValue() shouldBe """{"abc":123,"def":4}"""
    }

    @Test fun `should return elidedValue for object with exclusions`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        json.elidedValue() shouldBe """{"aaa":111,"bbb":222,"ccc":333,"ddd":444,"eee":555}"""
        json.elidedValue(exclude = emptyList()) shouldBe """{"aaa":111,"bbb":222,"ccc":333,"ddd":444,"eee":555}"""
        json.elidedValue(exclude = setOf("ddd")) shouldBe """{"aaa":111,"bbb":222,"ccc":333,"ddd":"****","eee":555}"""
        json.elidedValue(exclude = setOf("ddd", "eee")) shouldBe
                """{"aaa":111,"bbb":222,"ccc":333,"ddd":"****","eee":"****"}"""
    }

    @Test fun `should return elidedValue for object with inclusions`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        json.elidedValue(include = emptyList()) shouldBe
                """{"aaa":"****","bbb":"****","ccc":"****","ddd":"****","eee":"****"}"""
        json.elidedValue(include = setOf("ddd")) shouldBe
                """{"aaa":"****","bbb":"****","ccc":"****","ddd":444,"eee":"****"}"""
        json.elidedValue(include = setOf("ddd", "eee")) shouldBe
                """{"aaa":"****","bbb":"****","ccc":"****","ddd":444,"eee":555}"""
    }

    @Test fun `should return elidedValue for object with custom substitute string`() {
        val json = JSONObject.build {
            add("aaa", 111)
            add("bbb", 222)
            add("ccc", 333)
            add("ddd", 444)
            add("eee", 555)
        }
        json.elidedValue(exclude = setOf("ddd"), substitute = "") shouldBe
                """{"aaa":111,"bbb":222,"ccc":333,"ddd":"","eee":555}"""
        json.elidedValue(exclude = setOf("ddd", "eee"), substitute = "elided\u2020") shouldBe
                """{"aaa":111,"bbb":222,"ccc":333,"ddd":"elided\u2020","eee":"elided\u2020"}"""
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
        JSONArray.of(obj1, obj2).elidedValue() shouldBe """[{"aaa":111,"bbb":222,"ccc":333},{"ddd":444,"eee":555}]"""
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
        JSONArray.of(obj1, obj2).elidedValue(exclude = setOf("ccc")) shouldBe
                """[{"aaa":111,"bbb":222,"ccc":"****"},{"ddd":444,"eee":555}]"""
    }

    @Test fun `should return asString for JSONString`() {
        val json = JSONString("boring")
        json.asString shouldBe "boring"
        json.asStringOrNull shouldBe "boring"
        json.asStringOrError("string", 333, "Item") shouldBe "boring"
        json.asStringOr { "wrong" } shouldBe "boring"
    }

    @Test fun `should fail on attempt to get asString of other types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asStringOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (String), was 8") {
            jsonInt.asString
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "String"
            it.key shouldBe null
            it.value shouldBe jsonInt
        }
        jsonInt.asStringOr { "wrong" } shouldBe "wrong"
        val jsonArray = JSONArray.of(JSONString("Testing"))
        shouldThrow<JSONTypeException>("Node not correct type (String), was [ ... ]") {
            jsonArray.asString
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "String"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asStringOr { "wrong" } shouldBe "wrong"
    }

    @Test fun `should throw custom error on attempt to get asStringOrError of other types`() {
        val jsonInt = JSONInt(8)
        shouldThrow<JSONTypeException>("Value not correct type (string), was 8, at test1") {
            jsonInt.asStringOrError("string", "test1", "Value")
        }.let {
            it.nodeName shouldBe "Value"
            it.expected shouldBe "string"
            it.key shouldBe "test1"
            it.value shouldBe jsonInt
        }
    }

    @Test fun `should return asInt for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asInt shouldBe 8
        jsonInt.asIntOrNull shouldBe 8
        jsonInt.asIntOrError("xxx", "yyy", "zzz") shouldBe 8
        jsonInt.asIntOr { 999 } shouldBe 8
        val jsonLong = JSONLong(12345)
        jsonLong.asInt shouldBe 12345
        jsonLong.asIntOrNull shouldBe 12345
        jsonLong.asIntOrError(key = 27) shouldBe 12345
        jsonLong.asIntOr { 999 } shouldBe 12345
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asInt shouldBe 123
        jsonDecimal.asIntOrNull shouldBe 123
        jsonDecimal.asIntOrError("aaa") shouldBe 123
        jsonDecimal.asIntOr { 999 } shouldBe 123
    }

    @Test fun `should fail on attempt to get asInt of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Int), was \"not a number\"") {
            jsonString.asInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Int"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asIntOr { 999 } shouldBe 999
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Int), was [ ... ]") {
            jsonArray.asInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Int"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asIntOr { 999 } shouldBe 999
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Int), was 123.5000") {
            jsonDecimal.asInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Int"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asIntOr { 999 } shouldBe 999
    }

    @Test fun `should throw custom error on attempt to get asIntOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Value not correct type (integer), was \"not a number\", at test") {
            jsonString.asIntOrError("integer", "test", "Value")
        }.let {
            it.nodeName shouldBe "Value"
            it.expected shouldBe "integer"
            it.key shouldBe "test"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asLong for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asLong shouldBe 8
        jsonInt.asLongOrNull shouldBe 8
        jsonInt.asLongOr { -1 } shouldBe 8
        val jsonLong = JSONLong(1234567812345678)
        jsonLong.asLong shouldBe 1234567812345678
        jsonLong.asLongOrNull shouldBe 1234567812345678
        jsonLong.asLongOrError("long integer", 999) shouldBe 1234567812345678
        jsonLong.asLongOr { -1 } shouldBe 1234567812345678
        val jsonDecimal = JSONDecimal("9876543219876543.0000".toBigDecimal())
        jsonDecimal.asLong shouldBe 9876543219876543
        jsonDecimal.asLongOrNull shouldBe 9876543219876543
        jsonDecimal.asLongOrError(key = 6, nodeName = "Property") shouldBe 9876543219876543
        jsonDecimal.asLongOr { -1 } shouldBe 9876543219876543
    }

    @Test fun `should fail on attempt to get asLong of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asLongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Long), was \"not a number\"") {
            jsonString.asLong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Long"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asLongOr { -1 } shouldBe -1
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        shouldThrow<JSONTypeException>("Node not correct type (Long), was { ... }") {
            jsonObject.asLong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Long"
            it.key shouldBe null
            it.value shouldBe jsonObject
        }
        jsonObject.asLongOr { -1 } shouldBe -1
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asLongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Long), was 123.5000") {
            jsonDecimal.asLong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Long"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asLongOr { -1 } shouldBe -1
    }

    @Test fun `should throw custom error on attempt to get asLongOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Value not correct type (long), was \"not a number\", at test") {
            jsonString.asLongOrError("long", "test", "Value")
        }.let {
            it.nodeName shouldBe "Value"
            it.expected shouldBe "long"
            it.key shouldBe "test"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asShort for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asShort shouldBe 8
        jsonInt.asShortOrNull shouldBe 8
        jsonInt.asShortOrError("short") shouldBe 8
        jsonInt.asShortOr { 9999 } shouldBe 8
        val jsonLong = JSONLong(12345)
        jsonLong.asShort shouldBe 12345
        jsonLong.asShortOrNull shouldBe 12345
        jsonLong.asShortOrError(key = "key") shouldBe 12345
        jsonLong.asShortOr { 9999 } shouldBe 12345
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asShort shouldBe 123
        jsonDecimal.asShortOrNull shouldBe 123
        jsonDecimal.asShortOrError("short", 1) shouldBe 123
        jsonDecimal.asShortOr { 9999 } shouldBe 123
    }

    @Test fun `should fail on attempt to get asShort of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Short), was \"not a number\"") {
            jsonString.asShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Short"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asShortOr { 9999 } shouldBe 9999
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Short), was [ ... ]") {
            jsonArray.asShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Short"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asShortOr { 9999 } shouldBe 9999
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Short), was 123.5000") {
            jsonDecimal.asShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Short"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asShortOr { 9999 } shouldBe 9999
    }

    @Test fun `should throw custom error on attempt to get asShortOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Value not correct type (short), was \"not a number\", at test99") {
            jsonString.asShortOrError("short", "test99", "Value")
        }.let {
            it.nodeName shouldBe "Value"
            it.expected shouldBe "short"
            it.key shouldBe "test99"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asByte for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asByte shouldBe 8
        jsonInt.asByteOrNull shouldBe 8
        jsonInt.asByteOrError() shouldBe 8
        jsonInt.asByteOr { 99 } shouldBe 8
        val jsonLong = JSONLong(123)
        jsonLong.asByte shouldBe 123
        jsonLong.asByteOrNull shouldBe 123
        jsonLong.asByteOrError("b", "c", "d") shouldBe 123
        jsonLong.asByteOr { 99 } shouldBe 123
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asByte shouldBe 123
        jsonDecimal.asByteOrNull shouldBe 123
        jsonDecimal.asByteOrError("byte", "test4") shouldBe 123
        jsonDecimal.asByteOr { 99 } shouldBe 123
    }

    @Test fun `should fail on attempt to get asByte of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Byte), was \"not a number\"") {
            jsonString.asByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Byte"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asShortOr { 99 } shouldBe 99
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Byte), was [ ... ]") {
            jsonArray.asByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Byte"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asShortOr { 99 } shouldBe 99
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Byte), was 123.5000") {
            jsonDecimal.asByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Byte"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asShortOr { 99 } shouldBe 99
    }

    @Test fun `should throw custom error on attempt to get asByteOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Item not correct type (byte), was \"not a number\", at test123") {
            jsonString.asByteOrError("byte", "test123", "Item")
        }.let {
            it.nodeName shouldBe "Item"
            it.expected shouldBe "byte"
            it.key shouldBe "test123"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asULong for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asULong shouldBe 8.toULong()
        jsonInt.asULongOrNull shouldBe 8.toULong()
        jsonInt.asULongOrError() shouldBe 8.toULong()
        jsonInt.asULongOr { 99999U } shouldBe 8.toULong()
        val jsonLong = JSONLong(12345)
        jsonLong.asULong shouldBe 12345.toULong()
        jsonLong.asULongOrNull shouldBe 12345.toULong()
        jsonLong.asULongOrError(key = 27) shouldBe 12345.toULong()
        jsonLong.asULongOr { 99999U } shouldBe 12345.toULong()
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asULong shouldBe 123.toULong()
        jsonDecimal.asULongOrNull shouldBe 123.toULong()
        jsonDecimal.asULongOrError("whatever") shouldBe 123.toULong()
        jsonDecimal.asULongOr { 99999U } shouldBe 123.toULong()
    }

    @Test fun `should fail on attempt to get asULong of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asULongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was \"not a number\"") {
            jsonString.asULong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asULongOr { 99999U } shouldBe 99999U
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asULongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was [ ... ]") {
            jsonArray.asULong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asULongOr { 99999U } shouldBe 99999U
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asULongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was 123.5000") {
            jsonDecimal.asULong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asULongOr { 99999U } shouldBe 99999U
        val jsonInt = JSONInt(-1)
        jsonInt.asULongOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was -1") {
            jsonInt.asULong
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe null
            it.value shouldBe jsonInt
        }
        jsonInt.asULongOr { 99999U } shouldBe 99999U
    }

    @Test fun `should throw custom error on attempt to get asULongOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Item not correct type (unsigned long), was \"not a number\", at test2") {
            jsonString.asULongOrError("unsigned long", "test2", "Item")
        }.let {
            it.nodeName shouldBe "Item"
            it.expected shouldBe "unsigned long"
            it.key shouldBe "test2"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asUInt for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asUInt shouldBe 8U
        jsonInt.asUIntOrNull shouldBe 8U
        jsonInt.asUIntOrError("stuff", 15, "Thing") shouldBe 8U
        jsonInt.asUIntOr { 9999U } shouldBe 8U
        val jsonLong = JSONLong(12345)
        jsonLong.asUInt shouldBe 12345U
        jsonLong.asUIntOrNull shouldBe 12345U
        jsonLong.asUIntOrError() shouldBe 12345U
        jsonLong.asUIntOr { 9999U } shouldBe 12345U
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asUInt shouldBe 123U
        jsonDecimal.asUIntOrNull shouldBe 123U
        jsonDecimal.asUIntOrError("unsigned int", "property", "Property") shouldBe 123U
        jsonDecimal.asUIntOr { 9999U } shouldBe 123U
    }

    @Test fun `should fail on attempt to get asUInt of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asUIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was \"not a number\"") {
            jsonString.asUInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asUIntOr { 9999U } shouldBe 9999U
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asUIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was [ ... ]") {
            jsonArray.asUInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asUIntOr { 9999U } shouldBe 9999U
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asUIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was 123.5000") {
            jsonDecimal.asUInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asUIntOr { 9999U } shouldBe 9999U
        val jsonInt = JSONInt(-1)
        jsonInt.asUIntOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was -1") {
            jsonInt.asUInt
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe null
            it.value shouldBe jsonInt
        }
        jsonInt.asUIntOr { 9999U } shouldBe 9999U
    }

    @Test fun `should throw custom error on attempt to get asUIntOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Item not correct type (unsigned int), was \"not a number\", at test1234") {
            jsonString.asUIntOrError("unsigned int", "test1234", "Item")
        }.let {
            it.nodeName shouldBe "Item"
            it.expected shouldBe "unsigned int"
            it.key shouldBe "test1234"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asUShort for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asUShort shouldBe 8U
        jsonInt.asUShortOrNull shouldBe 8U
        jsonInt.asUShortOrError("a", "b", "c") shouldBe 8U
        jsonInt.asUShortOr { 999U } shouldBe 8U
        val jsonLong = JSONLong(45678)
        jsonLong.asUShort shouldBe 45678U
        jsonLong.asUShortOrNull shouldBe 45678U
        jsonLong.asUShortOrError() shouldBe 45678U
        jsonLong.asUShortOr { 999U } shouldBe 45678U
        val jsonDecimal = JSONDecimal("1234.0000".toBigDecimal())
        jsonDecimal.asUShort shouldBe 1234U
        jsonDecimal.asUShortOrNull shouldBe 1234U
        jsonDecimal.asUShortOrError(key = "it") shouldBe 1234U
        jsonDecimal.asUShortOr { 999U } shouldBe 1234U
    }

    @Test fun `should fail on attempt to get asUShort of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asUShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was \"not a number\"") {
            jsonString.asUShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asUShortOr { 999U } shouldBe 999U
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asUShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was [ ... ]") {
            jsonArray.asUShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asUShortOr { 999U } shouldBe 999U
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asUShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was 123.5000") {
            jsonDecimal.asUShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asUShortOr { 999U } shouldBe 999U
        val jsonInt = JSONInt(-1)
        jsonInt.asUShortOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was -1") {
            jsonInt.asUShort
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe null
            it.value shouldBe jsonInt
        }
        jsonInt.asUShortOr { 999U } shouldBe 999U
    }

    @Test fun `should throw custom error on attempt to get asUShortOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Item not correct type (unsigned short), was \"not a number\", at testtest") {
            jsonString.asUShortOrError("unsigned short", "testtest", "Item")
        }.let {
            it.nodeName shouldBe "Item"
            it.expected shouldBe "unsigned short"
            it.key shouldBe "testtest"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asUByte for number types`() {
        val jsonInt = JSONInt(8)
        jsonInt.asUByte shouldBe 8U
        jsonInt.asUByteOrNull shouldBe 8U
        jsonInt.asUByteOrError("unsigned byte", nodeName = "Item") shouldBe 8U
        jsonInt.asUByteOr { 99U } shouldBe 8U
        val jsonLong = JSONLong(234)
        jsonLong.asUByte shouldBe 234U
        jsonLong.asUByteOrNull shouldBe 234U
        jsonLong.asUByteOrError() shouldBe 234U
        jsonLong.asUByteOr { 99U } shouldBe 234U
        val jsonDecimal = JSONDecimal("123.0000".toBigDecimal())
        jsonDecimal.asUByte shouldBe 123U
        jsonDecimal.asUByteOrNull shouldBe 123U
        jsonDecimal.asUByteOrError("type", "key", "name") shouldBe 123U
        jsonDecimal.asUByteOr { 99U } shouldBe 123U
    }

    @Test fun `should fail on attempt to get asUByte of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asUByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was \"not a number\"") {
            jsonString.asUByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asUByteOr { 99U } shouldBe 99U
        val jsonArray = JSONArray.of(JSONString("Testing"))
        jsonArray.asUByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was [ ... ]") {
            jsonArray.asUByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe null
            it.value shouldBe jsonArray
        }
        jsonArray.asUByteOr { 99U } shouldBe 99U
        val jsonDecimal = JSONDecimal("123.5000".toBigDecimal())
        jsonDecimal.asUByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was 123.5000") {
            jsonDecimal.asUByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe null
            it.value shouldBe jsonDecimal
        }
        jsonDecimal.asUByteOr { 99U } shouldBe 99U
        val jsonInt = JSONInt(-1)
        jsonInt.asUByteOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was -1") {
            jsonInt.asUByte
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe null
            it.value shouldBe jsonInt
        }
        jsonInt.asUByteOr { 99U } shouldBe 99U
    }

    @Test fun `should throw custom error on attempt to get asUByteOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Node not correct type (unsigned byte), was \"not a number\", at test333") {
            jsonString.asUByteOrError("unsigned byte", "test333")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "unsigned byte"
            it.key shouldBe "test333"
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asDecimal for number types`() {
        8.let {
            val result = it.toBigDecimal()
            val jsonInt = JSONInt(it)
            jsonInt.asDecimal shouldBe result
            jsonInt.asDecimalOrNull shouldBe result
            jsonInt.asDecimalOrError("a", "b", "c") shouldBe result
            jsonInt.asDecimalOr { BigDecimal.TEN } shouldBe result
        }
        1234567812345678.let {
            val result = it.toBigDecimal()
            val jsonLong = JSONLong(it)
            jsonLong.asDecimal shouldBe result
            jsonLong.asDecimalOrNull shouldBe result
            jsonLong.asDecimalOrError(key = 9) shouldBe result
            jsonLong.asDecimalOr { BigDecimal.TEN } shouldBe result
        }
        "123.45678".let {
            val result = it.toBigDecimal()
            val jsonDecimal = JSONDecimal(result)
            jsonDecimal.asDecimal shouldBe result
            jsonDecimal.asDecimalOrNull shouldBe result
            jsonDecimal.asDecimalOrError() shouldBe result
            jsonDecimal.asDecimalOr { BigDecimal.TEN } shouldBe result
        }
    }

    @Test fun `should fail on attempt to get asDecimal of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asDecimalOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (BigDecimal), was \"not a number\"") {
            jsonString.asDecimal
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "BigDecimal"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asDecimalOr { BigDecimal.TEN } shouldBe BigDecimal.TEN
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        shouldThrow<JSONTypeException>("Node not correct type (BigDecimal), was { ... }") {
            jsonObject.asDecimal
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "BigDecimal"
            it.key shouldBe null
            it.value shouldBe jsonObject
        }
        jsonObject.asDecimalOr { BigDecimal.TEN } shouldBe BigDecimal.TEN
    }

    @Test fun `should throw custom error on attempt to get asDecimalOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Property not correct type (decimal number), was \"not a number\", at 1000") {
            jsonString.asDecimalOrError("decimal number", 1000, "Property")
        }.let {
            it.nodeName shouldBe "Property"
            it.expected shouldBe "decimal number"
            it.key shouldBe 1000
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asNumber for number types`() {
        8.let {
            val jsonInt = JSONInt(it)
            jsonInt.asNumber shouldBe jsonInt
            jsonInt.asNumberOrNull shouldBe jsonInt
            jsonInt.asNumberOrError("a", "b", "c") shouldBe jsonInt
            jsonInt.asNumberOr { BigDecimal.TEN } shouldBe jsonInt
        }
        1234567812345678.let {
            val jsonLong = JSONLong(it)
            jsonLong.asNumber shouldBe jsonLong
            jsonLong.asNumberOrNull shouldBe jsonLong
            jsonLong.asNumberOrError(key = 9) shouldBe jsonLong
            jsonLong.asNumberOr { BigDecimal.TEN } shouldBe jsonLong
        }
        "123.45678".let {
            val jsonDecimal = JSONDecimal(it)
            jsonDecimal.asNumber shouldBe jsonDecimal
            jsonDecimal.asNumberOrNull shouldBe jsonDecimal
            jsonDecimal.asNumberOrError() shouldBe jsonDecimal
            jsonDecimal.asNumberOr { BigDecimal.TEN } shouldBe jsonDecimal
        }
    }

    @Test fun `should fail on attempt to get asNumber of other types`() {
        val jsonString = JSONString("not a number")
        jsonString.asNumberOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Number), was \"not a number\"") {
            jsonString.asNumber
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Number"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asNumberOr { BigDecimal.TEN } shouldBe BigDecimal.TEN
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        shouldThrow<JSONTypeException>("Node not correct type (Number), was { ... }") {
            jsonObject.asNumber
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Number"
            it.key shouldBe null
            it.value shouldBe jsonObject
        }
        jsonObject.asNumberOr { BigDecimal.TEN } shouldBe BigDecimal.TEN
    }

    @Test fun `should throw custom error on attempt to get asNumberOrError of other types`() {
        val jsonString = JSONString("not a number")
        shouldThrow<JSONTypeException>("Property not correct type (number), was \"not a number\", at 1000") {
            jsonString.asNumberOrError("number", 1000, "Property")
        }.let {
            it.nodeName shouldBe "Property"
            it.expected shouldBe "number"
            it.key shouldBe 1000
            it.value shouldBe jsonString
        }
    }

    @Test fun `should return asBoolean for JSONBoolean`() {
        JSONBoolean.TRUE.asBoolean shouldBe true
        JSONBoolean.TRUE.asBooleanOrNull shouldBe true
        JSONBoolean.TRUE.asBooleanOrError("boolean") shouldBe true
        JSONBoolean.TRUE.asBooleanOr { false } shouldBe true
        JSONBoolean.FALSE.asBoolean shouldBe false
        JSONBoolean.FALSE.asBooleanOrNull shouldBe false
        JSONBoolean.FALSE.asBooleanOrError(key = 0) shouldBe false
        JSONBoolean.FALSE.asBooleanOr { true } shouldBe false
    }

    @Test fun `should fail on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        jsonString.asBooleanOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (Boolean), was \"not a boolean\"") {
            jsonString.asBoolean
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Boolean"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asBooleanOr { false } shouldBe false
    }

    @Test fun `should throw custom error on attempt to get asBoolean of other types`() {
        val jsonString = JSONString("not a boolean")
        shouldThrow<JSONTypeException>("Flag not correct type (boolean), was \"not a boolean\", at aaa") {
            jsonString.asBooleanOrError("boolean", "aaa", "Flag")
        }.let {
            it.nodeName shouldBe "Flag"
            it.expected shouldBe "boolean"
            it.key shouldBe "aaa"
            it.value shouldBe jsonString
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asArray for JSONArray`() {
        val jsonArray = JSONArray.of(JSONInt(123), JSONInt(456))
        jsonArray.asArray shouldBeSameInstance jsonArray
        jsonArray.asArrayOrNull shouldBeSameInstance jsonArray
        jsonArray.asArrayOrError("array") shouldBeSameInstance jsonArray
        jsonArray.asArrayOr { JSONArray.of(JSONInt(99)) } shouldBeSameInstance jsonArray
    }

    @Test fun `should fail on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        jsonString.asArrayOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (JSONArray), was \"not an array\"") {
            jsonString.asArray
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONArray"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asArrayOr { JSONArray.of(JSONInt(99)) } shouldBe JSONArray.of(JSONInt(99))
    }

    @Test fun `should throw custom error on attempt to get asArray of other types`() {
        val jsonString = JSONString("not an array")
        shouldThrow<JSONTypeException>("Node not correct type (array), was \"not an array\"") {
            jsonString.asArrayOrError("array")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "array"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
    }

    @Suppress("DEPRECATION")
    @Test fun `should return asObject for JSONObject`() {
        val jsonObject = JSONObject.of("name" to JSONString("value"))
        jsonObject.asObject shouldBeSameInstance jsonObject
        jsonObject.asObjectOrNull shouldBeSameInstance jsonObject
        jsonObject.asObjectOrError("object") shouldBeSameInstance jsonObject
        jsonObject.asObjectOr { JSONObject.of("error" to JSONInt(999)) } shouldBeSameInstance jsonObject
    }

    @Test fun `should fail on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        jsonString.asObjectOrNull shouldBe null
        shouldThrow<JSONTypeException>("Node not correct type (JSONObject), was \"not an object\"") {
            jsonString.asObject
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONObject"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
        jsonString.asObjectOr { JSONObject.of("error" to JSONInt(999)) } shouldBe JSONObject.of("error" to JSONInt(999))
    }

    @Test fun `should throw custom error on attempt to get asObject of other types`() {
        val jsonString = JSONString("not an object")
        shouldThrow<JSONTypeException>("Array not correct type (object), was \"not an object\"") {
            jsonString.asObjectOrError("object", nodeName = "Array")
        }.let {
            it.nodeName shouldBe "Array"
            it.expected shouldBe "object"
            it.key shouldBe null
            it.value shouldBe jsonString
        }
    }

    @Test fun `should throw general type error`() {
        val json = JSONString("this is a string")
        shouldThrow<JSONTypeException>("Node not correct type (integer), was \"this is a string\"") {
            if (json.isNotEmpty()) // dummy check added to stop editor from flagging "unreachable code"
                json.typeError("integer")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "integer"
            it.key shouldBe null
            it.value shouldBe json
        }
    }

    @Test fun `should produce structures capable of being formatted by json-simple - 1`() {
        val input = """{"aaa":1,"bbb":true,"ccc":"\u2014","ddd":1.0,"eee":{},"fff":[],"ggg":null}"""
        val json = JSON.parse(input)
        val formatter = Formatter(unixLineSeparator)
        val sb = StringBuilder()
        formatter.formatTo(sb, json)
        sb.toString() shouldBe formatted1
        Formatter.output(json) shouldBe input
    }

    @Test fun `should produce structures capable of being formatted by json-simple - 2`() {
        val input = """{"aaa":{"bbb":{"ccc":{"ddd":{},"eee":[1,2,3,4,5]}}}}"""
        val json = JSON.parse(input)
        val formatter = Formatter(unixLineSeparator)
        val formatted = buildString { formatter.formatTo(this, json) }
        formatted shouldBe formatted2
        Formatter.output(json) shouldBe input
    }

    @Suppress("ConstPropertyName")
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
