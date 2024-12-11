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

import java.math.BigDecimal

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeSameInstance
import io.kstuff.test.shouldBeType
import io.kstuff.test.shouldThrow

import io.kjson.JSONDecimal
import io.kjson.JSONInt
import io.kjson.JSONLong
import io.kjson.parser.ParserConstants.rootPointer
import io.kjson.parser.ParserErrors.EXCESS_CHARS
import io.kjson.parser.ParserErrors.ILLEGAL_NUMBER

class ParserNumberTest {

    @Test fun `should parse zero`() {
        val result = Parser.parse("0")
        result shouldBeSameInstance JSONInt.ZERO
    }

    @Test fun `should reject leading zeros`() {
        shouldThrow<ParseException>(ILLEGAL_NUMBER) { Parser.parse("00") }.let {
            it.text shouldBe ILLEGAL_NUMBER
            it.pointer shouldBe rootPointer
        }
        shouldThrow<ParseException>(ILLEGAL_NUMBER) { Parser.parse("0123") }.let {
            it.text shouldBe ILLEGAL_NUMBER
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should reject incorrect numbers`() {
        shouldThrow<ParseException>(EXCESS_CHARS) { Parser.parse("123a") }.let {
            it.text shouldBe EXCESS_CHARS
            it.pointer shouldBe rootPointer
        }
        shouldThrow<ParseException>(EXCESS_CHARS) { Parser.parse("12:00") }.let {
            it.text shouldBe EXCESS_CHARS
            it.pointer shouldBe rootPointer
        }
        shouldThrow<ParseException>(EXCESS_CHARS) { Parser.parse("1.23/4") }.let {
            it.text shouldBe EXCESS_CHARS
            it.pointer shouldBe rootPointer
        }
    }

    @Test fun `should parse positive integers`() {
        val result1 = Parser.parse("123")
        result1.shouldBeType<JSONInt>()
        result1.value shouldBe 123
        val result2 = Parser.parse("5678900")
        result2.shouldBeType<JSONInt>()
        result2.value shouldBe 5678900
        val result3 = Parser.parse("2147483647")
        result3.shouldBeType<JSONInt>()
        result3.value shouldBe 2147483647
    }

    @Test fun `should parse negative integers`() {
        val result1 = Parser.parse("-1")
        result1.shouldBeType<JSONInt>()
        result1.value shouldBe -1
        val result2 = Parser.parse("-876543")
        result2.shouldBeType<JSONInt>()
        result2.value shouldBe -876543
        val result3 = Parser.parse("-2147483648")
        result3.shouldBeType<JSONInt>()
        result3.value shouldBe -2147483648
    }

    @Test fun `should parse positive long integers`() {
        val result1 = Parser.parse("1234567890000")
        result1.shouldBeType<JSONLong>()
        result1.value shouldBe 1234567890000
        val result2 = Parser.parse("567895678956789")
        result2.shouldBeType<JSONLong>()
        result2.value shouldBe 567895678956789
        val result3 = Parser.parse("2147483648")
        result3.shouldBeType<JSONLong>()
        result3.value shouldBe 2147483648
        val result4 = Parser.parse("9223372036854775807")
        result4.shouldBeType<JSONLong>()
        result4.value shouldBe 9223372036854775807
    }

    @Test fun `should parse negative long integers`() {
        val result1 = Parser.parse("-1234567890000")
        result1.shouldBeType<JSONLong>()
        result1.value shouldBe -1234567890000
        val result2 = Parser.parse("-567895678956789")
        result2.shouldBeType<JSONLong>()
        result2.value shouldBe -567895678956789
        val result3 = Parser.parse("-2147483649")
        result3.shouldBeType<JSONLong>()
        result3.value shouldBe -2147483649
        val result4 = Parser.parse("-9223372036854775808")
        result4.shouldBeType<JSONLong>()
        result4.value shouldBe -9223372036854775807 - 1
    }

    @Test fun `should parse decimal`() {
        val result1 = Parser.parse("0.0")
        result1.shouldBeType<JSONDecimal>()
        result1.value.compareTo(BigDecimal.ZERO) shouldBe 0
        val result2 = Parser.parse("0.00")
        result2.shouldBeType<JSONDecimal>()
        result2.value.compareTo(BigDecimal.ZERO) shouldBe 0
    }

    @Test fun `should parse positive decimal`() {
        val result1 = Parser.parse("12340.0")
        result1.shouldBeType<JSONDecimal>()
        result1.value.compareTo("12340".toBigDecimal()) shouldBe 0
        val result2 = Parser.parse("1e200")
        result2.shouldBeType<JSONDecimal>()
        result2.value.compareTo("1e200".toBigDecimal()) shouldBe 0
        val result3 = Parser.parse("27e-60")
        result3.shouldBeType<JSONDecimal>()
        result3.value.compareTo("27e-60".toBigDecimal()) shouldBe 0
        val result4 = Parser.parse("0.1e-48")
        result4.shouldBeType<JSONDecimal>()
        result4.value.compareTo("0.1e-48".toBigDecimal()) shouldBe 0
        val result5 = Parser.parse("9223372036854775808")
        result5.shouldBeType<JSONDecimal>()
        result5.value.compareTo("9223372036854775808".toBigDecimal()) shouldBe 0
    }

    @Test fun `should parse negative decimal`() {
        val result1 = Parser.parse("-12340.0")
        result1.shouldBeType<JSONDecimal>()
        result1.value.compareTo("-12340".toBigDecimal()) shouldBe 0
        val result2 = Parser.parse("-1e200")
        result2.shouldBeType<JSONDecimal>()
        result2.value.compareTo("-1e200".toBigDecimal()) shouldBe 0
        val result3 = Parser.parse("-27e-60")
        result3.shouldBeType<JSONDecimal>()
        result3.value.compareTo("-27e-60".toBigDecimal()) shouldBe 0
        val result4 = Parser.parse("-0.1e-48")
        result4.shouldBeType<JSONDecimal>()
        result4.value.compareTo("-0.1e-48".toBigDecimal()) shouldBe 0
        val result5 = Parser.parse("-9223372036854775809")
        result5.shouldBeType<JSONDecimal>()
        result5.value.compareTo("-9223372036854775809".toBigDecimal()) shouldBe 0
    }

}
