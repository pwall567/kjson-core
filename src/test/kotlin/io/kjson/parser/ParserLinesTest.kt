/*
 * @(#) ParserLinesTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2023 Peter Wall
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
import kotlin.test.expect
import kotlin.test.assertFailsWith
import kotlin.test.assertIs

import io.kjson.JSON.asInt
import io.kjson.JSONArray
import io.kjson.JSONObject

class ParserLinesTest {

    @Test fun `should parse single line`() {
        val result = Parser.parseLines("""{"a":999}""")
        expect(1) { result.size }
        with(result[0]) {
            assertIs<JSONObject>(this)
            expect(1) { size }
            expect(999) { this["a"].asInt }
        }
    }

    @Test fun `should parse multiple lines`() {
        val result = Parser.parseLines("{\"a\":999}\n{\"b\":888}")
        expect(2) { result.size }
        with(result[0]) {
            assertIs<JSONObject>(this)
            expect(1) { size }
            expect(999) { this["a"].asInt }
        }
        with(result[1]) {
            assertIs<JSONObject>(this)
            expect(1) { size }
            expect(888) { this["b"].asInt }
        }
    }

    @Test fun `should parse multiple objects without newline`() {
        val result = Parser.parseLines("{\"a\":999}{\"b\":888}")
        expect(2) { result.size }
        with(result[0]) {
            assertIs<JSONObject>(this)
            expect(1) { size }
            expect(999) { this["a"].asInt }
        }
        with(result[1]) {
            assertIs<JSONObject>(this)
            expect(1) { size }
            expect(888) { this["b"].asInt }
        }
    }

    @Test fun `should parse empty file`() {
        val result = Parser.parseLines("")
        expect(0) { result.size }
    }

    @Test fun `should parse single line containing array`() {
        val result = Parser.parseLines("[123,456]")
        expect(1) { result.size }
        with(result[0]) {
            assertIs<JSONArray>(this)
            expect(123) { this[0].asInt }
            expect(456) { this[1].asInt }
        }
    }

    @Test fun `should parse multiple lines containing arrays`() {
        val result = Parser.parseLines("[123,456]\n[789]")
        expect(2) { result.size }
        with(result[0]) {
            assertIs<JSONArray>(this)
            expect(2) { size }
            expect(123) { this[0].asInt }
            expect(456) { this[1].asInt }
        }
        with(result[1]) {
            assertIs<JSONArray>(this)
            expect(1) { size }
            expect(789) { this[0].asInt }
        }
    }

    @Test fun `should ignore BOM if present`() {
        val result = Parser.parseLines("\uFEFF{}")
        expect(1) { result.size }
        with(result[0]) {
            assertIs<JSONObject>(this)
            expect(0) { size }
        }
    }

    @Test fun `should show correct pointer on errors`() {
        assertFailsWith<ParseException> { Parser.parseLines("""{} {"aaa":[xxx]}""") }.let {
            expect("Illegal JSON syntax, at /1/aaa/0") { it.message }
            expect("/1/aaa/0") { it.pointer }
        }
    }

}
