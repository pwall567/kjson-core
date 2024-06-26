/*
 * @(#) ParserNumberTest.kt
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
import kotlin.test.assertFailsWith
import kotlin.test.assertIs
import kotlin.test.assertSame
import kotlin.test.expect

import java.math.BigDecimal

import io.kjson.JSONDecimal
import io.kjson.JSONInt
import io.kjson.JSONLong
import io.kjson.parser.ParserConstants.rootPointer
import io.kjson.parser.ParserErrors.EXCESS_CHARS
import io.kjson.parser.ParserErrors.ILLEGAL_NUMBER

class ParserNumberTest {

    @Test fun `should parse zero`() {
        val result = Parser.parse("0")
        assertSame(JSONInt.ZERO, result)
    }

    @Test fun `should reject leading zeros`() {
        assertFailsWith<ParseException> { Parser.parse("00") }.let {
            expect(ILLEGAL_NUMBER) { it.text }
            expect(ILLEGAL_NUMBER) { it.message }
            expect(rootPointer) { it.pointer }
        }
        assertFailsWith<ParseException> { Parser.parse("0123") }.let {
            expect(ILLEGAL_NUMBER) { it.text }
            expect(ILLEGAL_NUMBER) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should reject incorrect numbers`() {
        assertFailsWith<ParseException> { Parser.parse("123a") }.let {
            expect(EXCESS_CHARS) { it.text }
            expect(EXCESS_CHARS) { it.message }
            expect(rootPointer) { it.pointer }
        }
        assertFailsWith<ParseException> { Parser.parse("12:00") }.let {
            expect(EXCESS_CHARS) { it.text }
            expect(EXCESS_CHARS) { it.message }
            expect(rootPointer) { it.pointer }
        }
        assertFailsWith<ParseException> { Parser.parse("1.23/4") }.let {
            expect(EXCESS_CHARS) { it.text }
            expect(EXCESS_CHARS) { it.message }
            expect(rootPointer) { it.pointer }
        }
    }

    @Test fun `should parse positive integers`() {
        val result1 = Parser.parse("123")
        assertIs<JSONInt>(result1)
        expect(123) { result1.value }
        val result2 = Parser.parse("5678900")
        assertIs<JSONInt>(result2)
        expect(5678900) { result2.value }
        val result3 = Parser.parse("2147483647")
        assertIs<JSONInt>(result3)
        expect(2147483647) { result3.value }
    }

    @Test fun `should parse negative integers`() {
        val result1 = Parser.parse("-1")
        assertIs<JSONInt>(result1)
        expect(-1) { result1.value }
        val result2 = Parser.parse("-876543")
        assertIs<JSONInt>(result2)
        expect(-876543) { result2.value }
        val result3 = Parser.parse("-2147483648")
        assertIs<JSONInt>(result3)
        expect(-2147483648) { result3.value }
    }

    @Test fun `should parse positive long integers`() {
        val result1 = Parser.parse("1234567890000")
        assertIs<JSONLong>(result1)
        expect(1234567890000) { result1.value }
        val result2 = Parser.parse("567895678956789")
        assertIs<JSONLong>(result2)
        expect(567895678956789) { result2.value }
        val result3 = Parser.parse("2147483648")
        assertIs<JSONLong>(result3)
        expect(2147483648) { result3.value }
        val result4 = Parser.parse("9223372036854775807")
        assertIs<JSONLong>(result4)
        expect(9223372036854775807) { result4.value }
    }

    @Test fun `should parse negative long integers`() {
        val result1 = Parser.parse("-1234567890000")
        assertIs<JSONLong>(result1)
        expect(-1234567890000) { result1.value }
        val result2 = Parser.parse("-567895678956789")
        assertIs<JSONLong>(result2)
        expect(-567895678956789) { result2.value }
        val result3 = Parser.parse("-2147483649")
        assertIs<JSONLong>(result3)
        expect(-2147483649) { result3.value }
        val result4 = Parser.parse("-9223372036854775808")
        assertIs<JSONLong>(result4)
        expect(-9223372036854775807 - 1) { result4.value }
    }

    @Test fun `should parse decimal`() {
        val result1 = Parser.parse("0.0")
        assertIs<JSONDecimal>(result1)
        expect(0) { result1.value.compareTo(BigDecimal.ZERO) }
        val result2 = Parser.parse("0.00")
        assertIs<JSONDecimal>(result2)
        expect(0) { result2.value.compareTo(BigDecimal.ZERO) }
    }

    @Test fun `should parse positive decimal`() {
        val result1 = Parser.parse("12340.0")
        assertIs<JSONDecimal>(result1)
        expect(0) { result1.value.compareTo("12340".toBigDecimal()) }
        val result2 = Parser.parse("1e200")
        assertIs<JSONDecimal>(result2)
        expect(0) { result2.value.compareTo("1e200".toBigDecimal()) }
        val result3 = Parser.parse("27e-60")
        assertIs<JSONDecimal>(result3)
        expect(0) { result3.value.compareTo("27e-60".toBigDecimal()) }
        val result4 = Parser.parse("0.1e-48")
        assertIs<JSONDecimal>(result4)
        expect(0) { result4.value.compareTo("0.1e-48".toBigDecimal()) }
        val result5 = Parser.parse("9223372036854775808")
        assertIs<JSONDecimal>(result5)
        expect(0) { result5.value.compareTo("9223372036854775808".toBigDecimal()) }
    }

    @Test fun `should parse negative decimal`() {
        val result1 = Parser.parse("-12340.0")
        assertIs<JSONDecimal>(result1)
        expect(0) { result1.value.compareTo("-12340".toBigDecimal()) }
        val result2 = Parser.parse("-1e200")
        assertIs<JSONDecimal>(result2)
        expect(0) { result2.value.compareTo("-1e200".toBigDecimal()) }
        val result3 = Parser.parse("-27e-60")
        assertIs<JSONDecimal>(result3)
        expect(0) { result3.value.compareTo("-27e-60".toBigDecimal()) }
        val result4 = Parser.parse("-0.1e-48")
        assertIs<JSONDecimal>(result4)
        expect(0) { result4.value.compareTo("-0.1e-48".toBigDecimal()) }
        val result5 = Parser.parse("-9223372036854775809")
        assertIs<JSONDecimal>(result5)
        expect(0) { result5.value.compareTo("-9223372036854775809".toBigDecimal()) }
    }

}
