/*
 * @(#) ParserLinesTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2023, 2024 Peter Wall
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
import io.kjson.JSONArray
import io.kjson.JSONObject

class ParserLinesTest {

    @Test fun `should parse single line`() {
        val result = Parser.parseLines("""{"a":999}""")
        result.size shouldBe 1
        with(result[0]) {
            shouldBeType<JSONObject>()
            size shouldBe 1
            this["a"].asInt shouldBe 999
        }
    }

    @Test fun `should parse multiple lines`() {
        val result = Parser.parseLines("{\"a\":999}\n{\"b\":888}")
        result.size shouldBe 2
        with(result[0]) {
            shouldBeType<JSONObject>()
            size shouldBe 1
            this["a"].asInt shouldBe 999
        }
        with(result[1]) {
            shouldBeType<JSONObject>()
            size shouldBe 1
            this["b"].asInt shouldBe 888
        }
    }

    @Test fun `should parse multiple objects without newline`() {
        val result = Parser.parseLines("{\"a\":999}{\"b\":888}")
        result.size shouldBe 2
        with(result[0]) {
            shouldBeType<JSONObject>()
            size shouldBe 1
            this["a"].asInt shouldBe 999
        }
        with(result[1]) {
            shouldBeType<JSONObject>()
            size shouldBe 1
            this["b"].asInt shouldBe 888
        }
    }

    @Test fun `should parse empty file`() {
        val result = Parser.parseLines("")
        result.size shouldBe 0
    }

    @Test fun `should parse single line containing array`() {
        val result = Parser.parseLines("[123,456]")
        result.size shouldBe 1
        with(result[0]) {
            shouldBeType<JSONArray>()
            this[0].asInt shouldBe 123
            this[1].asInt shouldBe 456
        }
    }

    @Test fun `should parse multiple lines containing arrays`() {
        val result = Parser.parseLines("[123,456]\n[789]")
        result.size shouldBe 2
        with(result[0]) {
            shouldBeType<JSONArray>()
            size shouldBe 2
            this[0].asInt shouldBe 123
            this[1].asInt shouldBe 456
        }
        with(result[1]) {
            shouldBeType<JSONArray>()
            size shouldBe 1
            this[0].asInt shouldBe 789
        }
    }

    @Test fun `should ignore BOM if present`() {
        val result = Parser.parseLines("\uFEFF{}")
        result.size shouldBe 1
        with(result[0]) {
            shouldBeType<JSONObject>()
            size shouldBe 0
        }
    }

    @Test fun `should show correct pointer on errors`() {
        shouldThrow<ParseException>("Illegal JSON syntax, at /1/aaa/0") {
            Parser.parseLines("""{} {"aaa":[xxx]}""")
        }.let {
            it.pointer shouldBe "/1/aaa/0"
        }
    }

}
