/*
 * @(#) ParserArrayTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2024 Peter Wall
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
        result.shouldBeType<JSONArray>()
        result.size shouldBe 0
    }

    @Test fun `should parse array of string`() {
        val result = Parser.parse("""["simple"]""")
        result.shouldBeType<JSONArray>()
        result.size shouldBe 1
        result[0] shouldBe JSONString("simple")
    }

    @Test fun `should parse array of two strings`() {
        val result = Parser.parse("""["Hello","world"]""")
        result.shouldBeType<JSONArray>()
        result.size shouldBe 2
        result[0] shouldBe JSONString("Hello")
        result[1] shouldBe JSONString("world")
    }

    @Test fun `should parse array of arrays`() {
        val result = Parser.parse("""["Hello",["world","universe"]]""")
        result.shouldBeType<JSONArray>()
        result.size shouldBe 2
        result[0] shouldBe JSONString("Hello")
        val inner = result[1]
        inner.shouldBeType<JSONArray>()
        inner.size shouldBe 2
        inner[0] shouldBe JSONString("world")
        inner[1] shouldBe JSONString("universe")
    }

    @Test fun `should throw exception on trailing comma`() {
        shouldThrow<ParseException>("$ILLEGAL_SYNTAX, at /1") { Parser.parse("""["simple",]""") }.let {
            it.text shouldBe ILLEGAL_SYNTAX
            it.pointer shouldBe "/1"
        }
    }

    @Test fun `should allow trailing comma with option arrayTrailingComma`() {
        val options = ParseOptions(arrayTrailingComma = true)
        val result = Parser.parse("""["simple",]""", options)
        result.shouldBeType<JSONArray>()
        result.size shouldBe 1
        result[0] shouldBe JSONString("simple")
    }

    @Test fun `should throw exception on missing closing bracket`() {
        shouldThrow<ParseException>(MISSING_CLOSING_BRACKET) { Parser.parse("""["simple"""") }.let {
            it.text shouldBe MISSING_CLOSING_BRACKET
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on syntax error`() {
        shouldThrow<ParseException>("$ILLEGAL_SYNTAX, at /0") { Parser.parse("""[&]""") }.let {
            it.text shouldBe ILLEGAL_SYNTAX
            it.pointer shouldBe "/0"
        }
    }

    @Test fun `should allow nesting up to maximum depth`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val allowed = "[".repeat(options.maximumNestingDepth) + '1' + "]".repeat(options.maximumNestingDepth)
        var result = Parser.parse(allowed, options)
        for (i in 0 until options.maximumNestingDepth) {
            result.shouldBeType<JSONArray>()
            result = result[0]
        }
        result shouldBe JSONInt(1)
    }

    @Test fun `should throw exception on nesting depth exceeded`() {
        val options = ParseOptions(maximumNestingDepth = 50)
        val excessive = "[".repeat(options.maximumNestingDepth + 1)
        shouldThrow<ParseException>(MAX_DEPTH_EXCEEDED) { Parser.parse(excessive, options) }.let {
            it.text shouldBe MAX_DEPTH_EXCEEDED
            it.pointer shouldBe rootPointer
        }
    }

}
