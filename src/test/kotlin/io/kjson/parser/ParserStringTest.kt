/*
 * @(#) ParserStringTest.kt
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

import io.kjson.JSONString
import io.kjson.parser.ParserConstants.rootPointer
import net.pwall.json.JSONFunctions

class ParserStringTest {

    @Test fun `should parse simple string`() {
        val result = Parser.parse("\"simple\"")
        result.shouldBeType<JSONString>()
        result.value shouldBe "simple"
    }

    @Test fun `should parse string with escape sequences`() {
        val result = Parser.parse("\"tab\\tnewline\\nquote\\\" \"")
        result.shouldBeType<JSONString>()
        result.value shouldBe "tab\tnewline\nquote\" "
    }

    @Test fun `should parse string with unicode escape sequences`() {
        val result = Parser.parse("\"mdash \\u2014\"")
        result.shouldBeType<JSONString>()
        result.value shouldBe "mdash \u2014"
    }

    @Test fun `should throw exception on missing closing quote`() {
        shouldThrow<ParseException>(JSONFunctions.UNTERMINATED_STRING) {
            Parser.parse("\"abc")
        }.let {
            it.text shouldBe JSONFunctions.UNTERMINATED_STRING
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on bad escape sequence`() {
        shouldThrow<ParseException>(JSONFunctions.ILLEGAL_ESCAPE_SEQUENCE) {
            Parser.parse("\"ab\\c\"")
        }.let {
            it.text shouldBe JSONFunctions.ILLEGAL_ESCAPE_SEQUENCE
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on bad unicode sequence`() {
        shouldThrow<ParseException>(JSONFunctions.ILLEGAL_UNICODE_SEQUENCE) {
            Parser.parse("\"ab\\uxxxx\"")
        }.let {
            it.text shouldBe JSONFunctions.ILLEGAL_UNICODE_SEQUENCE
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should throw exception on illegal character`() {
        shouldThrow<ParseException>(JSONFunctions.ILLEGAL_CHAR) {
            Parser.parse("\"ab\u0001\"")
        }.let {
            it.text shouldBe JSONFunctions.ILLEGAL_CHAR
            it.pointer shouldBe rootPointer
        }
        shouldThrow<ParseException>(JSONFunctions.ILLEGAL_CHAR) {
            Parser.parse("\"ab\n\"")
        }.let {
            it.text shouldBe JSONFunctions.ILLEGAL_CHAR
            it.pointer shouldBe rootPointer
        }
    }

}
