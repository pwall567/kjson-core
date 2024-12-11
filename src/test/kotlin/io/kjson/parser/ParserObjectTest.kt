/*
 * @(#) ParserObjectTest.kt
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

package io.kjson.parser

import kotlin.test.Test

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeType
import io.kstuff.test.shouldThrow

import io.kjson.JSON.asInt
import io.kjson.JSON.asObject
import io.kjson.JSONArray
import io.kjson.JSONException
import io.kjson.JSONInt
import io.kjson.JSONObject
import io.kjson.JSONString
import io.kjson.parser.ParserConstants.rootPointer
import io.kjson.parser.ParserErrors.ILLEGAL_KEY
import io.kjson.parser.ParserErrors.MAX_DEPTH_EXCEEDED
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACE
import io.kjson.parser.ParserErrors.MISSING_COLON

class ParserObjectTest {

    @Test fun `should parse empty object`() {
        val result = Parser.parse("{}")
        result.shouldBeType<JSONObject>()
        result.size shouldBe 0
    }

    @Test fun `should ignore BOM if present`() {
        val result = Parser.parse("\uFEFF{}")
        result.shouldBeType<JSONObject>()
        result.size shouldBe 0
    }

    @Test fun `should parse simple object`() {
        val result = Parser.parse("""{"first":123,"second":"Hi there!"}""")
        result.shouldBeType<JSONObject>()
        result.size shouldBe 2
        val first = result["first"]
        first.shouldBeType<JSONInt>()
        first.value shouldBe 123
        val second = result["second"]
        second.shouldBeType<JSONString>()
        second.value shouldBe "Hi there!"
    }

    @Test fun `should parse nested object`() {
        val result = Parser.parse("""{"first":123,"second":{"a":[{"aa":0}]}}""")
        result.shouldBeType<JSONObject>()
        result.size shouldBe 2
        val first = result["first"]
        first.shouldBeType<JSONInt>()
        first.value shouldBe 123
        val second = result["second"]
        second.shouldBeType<JSONObject>()
        second.size shouldBe 1
        val a = second["a"]
        a.shouldBeType<JSONArray>()
        a.size shouldBe 1
        val item = a[0]
        item.shouldBeType<JSONObject>()
        item.size shouldBe 1
        val aa = item["aa"]
        aa.shouldBeType<JSONInt>()
        aa.value shouldBe 0
    }

    @Test fun `should throw exception on missing closing brace`() {
        shouldThrow<ParseException>(MISSING_CLOSING_BRACE) { Parser.parse("""{"first":123""") }.let {
            it.text shouldBe MISSING_CLOSING_BRACE
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on missing colon`() {
        shouldThrow<ParseException>(MISSING_COLON) { Parser.parse("""{"first"123}""") }.let {
            it.text shouldBe MISSING_COLON
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on trailing comma`() {
        shouldThrow<ParseException>(ILLEGAL_KEY) { Parser.parse("""{"first":123,}""") }.let {
            it.text shouldBe ILLEGAL_KEY
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should allow trailing comma with option objectTrailingComma`() {
        val options = ParseOptions(objectTrailingComma = true)
        val result = Parser.parse("""{"first":123,}""", options)
        result.shouldBeType<JSONObject>()
        result.size shouldBe 1
        val first = result["first"]
        first.shouldBeType<JSONInt>()
        first.value shouldBe 123
    }

    @Test fun `should throw exception on missing quotes around key`() {
        shouldThrow<ParseException>(ILLEGAL_KEY) { Parser.parse("{first:123}") }.let {
            it.text shouldBe ILLEGAL_KEY
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should allow missing quotes around key with option objectKeyUnquoted`() {
        val options = ParseOptions(objectKeyUnquoted = true)
        val result = Parser.parse("{first:123}", options)
        result.shouldBeType<JSONObject>()
        result.size shouldBe 1
        val first = result["first"]
        first.shouldBeType<JSONInt>()
        first.value shouldBe 123
    }

    @Test fun `should throw exception on duplicate keys`() {
        shouldThrow<JSONException>("Duplicate key - first") { Parser.parse("""{"first":123,"first":456}""") }.let {
            it.text shouldBe "Duplicate key - first"
            it.key shouldBe ""
        }
    }

    @Test fun `should throw exception on duplicate keys with option ERROR`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.ERROR)
        shouldThrow<JSONException>("Duplicate key - first") {
            Parser.parse("""{"first":123,"first":456}""", options)
        }.let {
            it.text shouldBe "Duplicate key - first"
            it.key shouldBe ""
        }
    }

    @Test fun `should throw exception with pointer on duplicate keys with option ERROR`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.ERROR)
        shouldThrow<JSONException>("Duplicate key - first, at /abc") {
            Parser.parse("""{"abc":{"first":123,"first":456}}""", options)
        }.let {
            it.text shouldBe "Duplicate key - first"
            it.key shouldBe "/abc"
        }
    }

    @Test fun `should throw exception on duplicate keys with option CHECK_IDENTICAL and different values`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        shouldThrow<JSONException>("Duplicate key - first") {
            Parser.parse("""{"first":123,"first":456}""", options)
        }.let {
            it.text shouldBe "Duplicate key - first"
            it.key shouldBe ""
        }
    }

    @Test fun `should take first on duplicate keys with option CHECK_IDENTICAL and same values`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        Parser.parse("""{"first":123,"first":123}""", options).asObject.let {
            it.size shouldBe 1
            it["first"].asInt shouldBe 123
        }
    }

    @Test fun `should take first on duplicate keys with option TAKE_FIRST`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.TAKE_FIRST)
        Parser.parse("""{"first":123,"first":456}""", options).asObject.let {
            it.size shouldBe 1
            it["first"].asInt shouldBe 123
        }
    }

    @Test fun `should take last on duplicate keys with option TAKE_LAST`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.TAKE_LAST)
        Parser.parse("""{"first":123,"first":456}""", options).asObject.let {
            it.size shouldBe 1
            it["first"].asInt shouldBe 456
        }
    }

    @Test fun `should allow nesting up to maximum depth`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val allowed = """{"a":""".repeat(options.maximumNestingDepth) + '1' + "}".repeat(options.maximumNestingDepth)
        var result = Parser.parse(allowed, options)
        for (i in 0 until options.maximumNestingDepth) {
            result.shouldBeType<JSONObject>()
            result = result["a"]
        }
        result shouldBe JSONInt(1)
    }

    @Test fun `should throw exception on nesting depth exceeded`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val excessive = """{"a":""".repeat(options.maximumNestingDepth + 1)
        shouldThrow<ParseException>(MAX_DEPTH_EXCEEDED) { Parser.parse(excessive, options) }.let {
            it.text shouldBe MAX_DEPTH_EXCEEDED
            it.pointer shouldBe rootPointer
        }
    }

}
