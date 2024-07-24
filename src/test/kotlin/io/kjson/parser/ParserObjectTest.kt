/*
 * @(#) ParserObjectTest.kt
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

package io.kjson.parser

import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertIs
import kotlin.test.assertNull
import kotlin.test.expect

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
        assertIs<JSONObject>(result)
        expect(0) { result.size }
    }

    @Test fun `should ignore BOM if present`() {
        val result = Parser.parse("\uFEFF{}")
        assertIs<JSONObject>(result)
        expect(0) { result.size }
    }

    @Test fun `should parse simple object`() {
        val result = Parser.parse("""{"first":123,"second":"Hi there!"}""")
        assertIs<JSONObject>(result)
        expect(2) { result.size }
        val first = result["first"]
        assertIs<JSONInt>(first)
        expect(123) { first.value }
        val second = result["second"]
        assertIs<JSONString>(second)
        expect("Hi there!") { second.value }
    }

    @Test fun `should parse nested object`() {
        val result = Parser.parse("""{"first":123,"second":{"a":[{"aa":0}]}}""")
        assertIs<JSONObject>(result)
        expect(2) { result.size }
        val first = result["first"]
        assertIs<JSONInt>(first)
        expect(123) { first.value }
        val second = result["second"]
        assertIs<JSONObject>(second)
        expect(1) { second.size }
        val a = second["a"]
        assertIs<JSONArray>(a)
        expect(1) { a.size }
        val item = a[0]
        assertIs<JSONObject>(item)
        expect(1) { item.size }
        val aa = item["aa"]
        assertIs<JSONInt>(aa)
        expect(0) { aa.value }
    }

    @Test fun `should throw exception on missing closing brace`() {
        assertFailsWith<ParseException> { Parser.parse("""{"first":123""") }.let {
            expect(MISSING_CLOSING_BRACE) { it.text }
            expect(MISSING_CLOSING_BRACE) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on missing colon`() {
        assertFailsWith<ParseException> { Parser.parse("""{"first"123}""") }.let {
            expect(MISSING_COLON) { it.text }
            expect(MISSING_COLON) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on trailing comma`() {
        assertFailsWith<ParseException> { Parser.parse("""{"first":123,}""") }.let {
            expect(ILLEGAL_KEY) { it.text }
            expect(ILLEGAL_KEY) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should allow trailing comma with option objectTrailingComma`() {
        val options = ParseOptions(objectTrailingComma = true)
        val result = Parser.parse("""{"first":123,}""", options)
        assertIs<JSONObject>(result)
        expect(1) { result.size }
        val first = result["first"]
        assertIs<JSONInt>(first)
        expect(123) { first.value }
    }

    @Test fun `should throw exception on missing quotes around key`() {
        assertFailsWith<ParseException> { Parser.parse("{first:123}") }.let {
            expect(ILLEGAL_KEY) { it.text }
            expect(ILLEGAL_KEY) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should allow missing quotes around key with option objectKeyUnquoted`() {
        val options = ParseOptions(objectKeyUnquoted = true)
        val result = Parser.parse("{first:123}", options)
        assertIs<JSONObject>(result)
        expect(1) { result.size }
        val first = result["first"]
        assertIs<JSONInt>(first)
        expect(123) { first.value }
    }

    @Test fun `should throw exception on duplicate keys`() {
        assertFailsWith<JSONException> { Parser.parse("""{"first":123,"first":456}""") }.let {
            expect("Duplicate key - first") { it.text }
            expect("Duplicate key - first") { it.message }
            expect("") { it.key }
        }
    }

    @Test fun `should throw exception on duplicate keys with option ERROR`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.ERROR)
        assertFailsWith<JSONException> { Parser.parse("""{"first":123,"first":456}""", options) }.let {
            expect("Duplicate key - first") { it.text }
            expect("Duplicate key - first") { it.message }
            expect("") { it.key }
        }
    }

    @Test fun `should throw exception with pointer on duplicate keys with option ERROR`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.ERROR)
        assertFailsWith<JSONException> { Parser.parse("""{"abc":{"first":123,"first":456}}""", options) }.let {
            expect("Duplicate key - first") { it.text }
            expect("Duplicate key - first, at /abc") { it.message }
            expect("/abc") { it.key }
        }
    }

    @Test fun `should throw exception on duplicate keys with option CHECK_IDENTICAL and different values`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        assertFailsWith<JSONException> { Parser.parse("""{"first":123,"first":456}""", options) }.let {
            expect("Duplicate key - first") { it.text }
            expect("Duplicate key - first") { it.message }
            expect("") { it.key }
        }
    }

    @Test fun `should take first on duplicate keys with option CHECK_IDENTICAL and same values`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        Parser.parse("""{"first":123,"first":123}""", options).asObject.let {
            expect(1) { it.size }
            expect(123) { it["first"].asInt }
        }
    }

    @Test fun `should take first on duplicate keys with option TAKE_FIRST`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.TAKE_FIRST)
        Parser.parse("""{"first":123,"first":456}""", options).asObject.let {
            expect(1) { it.size }
            expect(123) { it["first"].asInt }
        }
    }

    @Test fun `should take last on duplicate keys with option TAKE_LAST`() {
        val options = ParseOptions(JSONObject.DuplicateKeyOption.TAKE_LAST)
        Parser.parse("""{"first":123,"first":456}""", options).asObject.let {
            expect(1) { it.size }
            expect(456) { it["first"].asInt }
        }
    }

    @Test fun `should allow nesting up to maximum depth`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val allowed = """{"a":""".repeat(options.maximumNestingDepth) + '1' + "}".repeat(options.maximumNestingDepth)
        var result = Parser.parse(allowed, options)
        for (i in 0 until options.maximumNestingDepth) {
            assertIs<JSONObject>(result)
            result = result["a"]
        }
        expect(JSONInt(1)) { result }
    }

    @Test fun `should throw exception on nesting depth exceeded`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val excessive = """{"a":""".repeat(options.maximumNestingDepth + 1)
        assertFailsWith<ParseException> { Parser.parse(excessive, options) }.let {
            expect(MAX_DEPTH_EXCEEDED) { it.text }
            expect(MAX_DEPTH_EXCEEDED) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

}
