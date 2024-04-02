/*
 * @(#) ParserArrayTest.kt
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

package io.kjson.parser

import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertIs
import kotlin.test.expect

import io.kjson.JSONArray
import io.kjson.JSONInt
import io.kjson.JSONString
import io.kjson.parser.ParserConstants.rootPointer
import io.kjson.parser.ParserErrors.ILLEGAL_SYNTAX
import io.kjson.parser.ParserErrors.MAX_DEPTH_EXCEEDED
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACKET

class ParserArrayTest {

    @Test fun `should parse empty array`() {
        val result = Parser.parse("[]")
        assertIs<JSONArray>(result)
        expect(0) { result.size }
    }

    @Test fun `should parse array of string`() {
        val result = Parser.parse("""["simple"]""")
        assertIs<JSONArray>(result)
        expect(1) { result.size }
        expect(JSONString("simple")) { result[0] }
    }

    @Test fun `should parse array of two strings`() {
        val result = Parser.parse("""["Hello","world"]""")
        assertIs<JSONArray>(result)
        expect(2) { result.size }
        expect(JSONString("Hello")) { result[0] }
        expect(JSONString("world")) { result[1] }
    }

    @Test fun `should parse array of arrays`() {
        val result = Parser.parse("""["Hello",["world","universe"]]""")
        assertIs<JSONArray>(result)
        expect(2) { result.size }
        expect(JSONString("Hello")) { result[0] }
        val inner = result[1]
        assertIs<JSONArray>(inner)
        expect(2) { inner.size }
        expect(JSONString("world")) { inner[0] }
        expect(JSONString("universe")) { inner[1] }
    }

    @Test fun `should throw exception on trailing comma`() {
        assertFailsWith<ParseException> { Parser.parse("""["simple",]""") }.let {
            expect(ILLEGAL_SYNTAX) { it.text }
            expect("$ILLEGAL_SYNTAX, at /1") { it.message }
            expect("/1") { it.pointer }
        }
    }

    @Test fun `should allow trailing comma with option arrayTrailingComma`() {
        val options = ParseOptions(arrayTrailingComma = true)
        val result = Parser.parse("""["simple",]""", options)
        assertIs<JSONArray>(result)
        expect(1) { result.size }
        expect(JSONString("simple")) { result[0] }
    }

    @Test fun `should throw exception on missing closing bracket`() {
        assertFailsWith<ParseException> { Parser.parse("""["simple"""") }.let {
            expect(MISSING_CLOSING_BRACKET) { it.text }
            expect(MISSING_CLOSING_BRACKET) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on syntax error`() {
        assertFailsWith<ParseException> { Parser.parse("""[&]""") }.let {
            expect(ILLEGAL_SYNTAX) { it.text }
            expect("$ILLEGAL_SYNTAX, at /0") { it.message }
            expect("/0") { it.pointer }
        }
    }

    @Test fun `should allow nesting up to maximum depth`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val allowed = "[".repeat(options.maximumNestingDepth) + '1' + "]".repeat(options.maximumNestingDepth)
        var result = Parser.parse(allowed, options)
        for (i in 0 until options.maximumNestingDepth) {
            assertIs<JSONArray>(result)
            result = result[0]
        }
        expect(JSONInt(1)) { result }
    }

    @Test fun `should throw exception on nesting depth exceeded`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val excessive = "[".repeat(options.maximumNestingDepth + 1)
        assertFailsWith<ParseException> { Parser.parse(excessive, options) }.let {
            expect(MAX_DEPTH_EXCEEDED) { it.text }
            expect(MAX_DEPTH_EXCEEDED) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

}
