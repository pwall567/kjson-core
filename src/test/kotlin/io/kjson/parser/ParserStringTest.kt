/*
 * @(#) ParserStringTest.kt
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

import io.kjson.JSONString
import io.kjson.parser.ParserConstants.rootPointer
import net.pwall.json.JSONFunctions

class ParserStringTest {

    @Test fun `should parse simple string`() {
        val result = Parser.parse("\"simple\"")
        assertIs<JSONString>(result)
        expect("simple") { result.value }
    }

    @Test fun `should parse string with escape sequences`() {
        val result = Parser.parse("\"tab\\tnewline\\nquote\\\" \"")
        assertIs<JSONString>(result)
        expect("tab\tnewline\nquote\" ") { result.value }
    }

    @Test fun `should parse string with unicode escape sequences`() {
        val result = Parser.parse("\"mdash \\u2014\"")
        assertIs<JSONString>(result)
        expect("mdash \u2014") { result.value }
    }

    @Test fun `should throw exception on missing closing quote`() {
        assertFailsWith<ParseException> { Parser.parse("\"abc") }.let {
            expect(JSONFunctions.UNTERMINATED_STRING) { it.text }
            expect(JSONFunctions.UNTERMINATED_STRING) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on bad escape sequence`() {
        assertFailsWith<ParseException> { Parser.parse("\"ab\\c\"") }.let {
            expect(JSONFunctions.ILLEGAL_ESCAPE_SEQUENCE) { it.text }
            expect(JSONFunctions.ILLEGAL_ESCAPE_SEQUENCE) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on bad unicode sequence`() {
        assertFailsWith<ParseException> { Parser.parse("\"ab\\uxxxx\"") }.let {
            expect(JSONFunctions.ILLEGAL_UNICODE_SEQUENCE) { it.text }
            expect(JSONFunctions.ILLEGAL_UNICODE_SEQUENCE) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should throw exception on illegal character`() {
        assertFailsWith<ParseException> { Parser.parse("\"ab\u0001\"") }.let {
            expect(JSONFunctions.ILLEGAL_CHAR) { it.text }
            expect(JSONFunctions.ILLEGAL_CHAR) { it.message }
            expect(rootPointer) { it.pointer }
        }
        assertFailsWith<ParseException> { Parser.parse("\"ab\n\"") }.let {
            expect(JSONFunctions.ILLEGAL_CHAR) { it.text }
            expect(JSONFunctions.ILLEGAL_CHAR) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

}
