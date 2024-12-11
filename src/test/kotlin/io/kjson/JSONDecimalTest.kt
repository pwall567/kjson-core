/*
 * @(#) JSONDecimalTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2024 Peter Wall
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

package io.kjson

import kotlin.test.Test

import kotlinx.coroutines.runBlocking

import java.math.BigDecimal

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeSameInstance

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONDecimalTest {

    @Test fun `should create JSONDecimal`() {
        JSONDecimal(BigDecimal.ZERO).let {
            it.value shouldBe BigDecimal.ZERO
            it.toJSON() shouldBe "0"
            it shouldBe JSONDecimal("0.00")
            it.shouldBe<JSONValue>(JSONInt(0))
            it.shouldBe<JSONValue>(JSONLong(0))
        }
    }

    @Test fun `should create JSONDecimal from Long`() {
        JSONDecimal(123456789123456789).let {
            it.value shouldBe 123456789123456789.toBigDecimal()
            it.toJSON() shouldBe "123456789123456789"
            it.shouldBe<JSONValue>(JSONLong(123456789123456789))
        }
    }

    @Test fun `should create JSONDecimal from Int`() {
        JSONDecimal(12345).let {
            it.value shouldBe 12345.toBigDecimal()
            it.toJSON() shouldBe "12345"
            it.shouldBe<JSONValue>(JSONInt(12345))
        }
    }

    @Test fun `should create JSONDecimal using of`() {
        JSONDecimal.of(BigDecimal.ZERO).let {
            it shouldBeSameInstance JSONDecimal.ZERO
            it.value shouldBe BigDecimal.ZERO
            it.toJSON() shouldBe "0"
            it shouldBe JSONDecimal("0.00")
        }
        JSONDecimal.of(BigDecimal.ONE).let {
            it.value shouldBe BigDecimal.ONE
            it.toJSON() shouldBe "1"
            it shouldBe JSONDecimal("1.00")
        }
        JSONDecimal.of(0) shouldBeSameInstance JSONDecimal.ZERO
        JSONDecimal.of(23456).let {
            it.value shouldBe "23456".toBigDecimal()
            it.toJSON() shouldBe "23456"
            it shouldBe JSONDecimal("23456")
        }
        JSONDecimal.of(0L) shouldBeSameInstance JSONDecimal.ZERO
        JSONDecimal.of(9876543210).let {
            it.value shouldBe "9876543210".toBigDecimal()
            it.toJSON() shouldBe "9876543210"
            it shouldBe JSONDecimal("9876543210")
        }
        JSONDecimal.of("0") shouldBeSameInstance JSONDecimal.ZERO
        JSONDecimal.of("333.5").let {
            it.value shouldBe "333.5".toBigDecimal()
            it.toJSON() shouldBe "333.5"
            it shouldBe JSONDecimal("333.5")
        }
    }

    @Test fun `should implement isXxxx functions`() {
        JSONDecimal(123456789123456789).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe false
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe true
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal(-123456789123456789).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe false
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal(123456789).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe true
            it.isUInt() shouldBe true
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal(-123456789).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal(12345).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe false
            it.isULong() shouldBe true
            it.isUInt() shouldBe true
            it.isUShort() shouldBe true
            it.isUByte() shouldBe false
        }
        JSONDecimal(-12345).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal(123).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe true
            it.isULong() shouldBe true
            it.isUInt() shouldBe true
            it.isUShort() shouldBe true
            it.isUByte() shouldBe true
        }
        JSONDecimal(-123).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe true
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal("0.123").let {
            it.isIntegral() shouldBe false
            it.isLong() shouldBe false
            it.isInt() shouldBe false
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal("-0.123").let {
            it.isIntegral() shouldBe false
            it.isLong() shouldBe false
            it.isInt() shouldBe false
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONDecimal("888.00").let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe false
            it.isULong() shouldBe true
            it.isUInt() shouldBe true
            it.isUShort() shouldBe true
            it.isUByte() shouldBe false
        }
        JSONDecimal("-888.00").let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe false
            it.isULong() shouldBe false
            it.isUInt() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
    }

    @Test fun `should implement isZero etc functions`() {
        JSONDecimal.ZERO.let {
            it.isZero() shouldBe true
            it.isPositive() shouldBe false
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe true
        }
        JSONDecimal(-123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe false
            it.isNegative() shouldBe true
            it.isNotNegative() shouldBe false
            it.isNotPositive() shouldBe true
        }
        JSONDecimal(123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe true
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe false
        }
    }

    @Test fun `should implement toDecimal`() {
        JSONDecimal.ZERO.toDecimal() shouldBe BigDecimal.ZERO
        JSONDecimal(12345).toDecimal() shouldBe 12345.toBigDecimal()
        JSONDecimal(-9).toDecimal() shouldBe (-9).toBigDecimal()
        JSONDecimal("567.89").toDecimal() shouldBe "567.89".toBigDecimal()
    }

    @Test fun `should implement toULong`() {
        JSONDecimal.ZERO.toULong() shouldBe 0U
        JSONDecimal(123456789123456789).toULong() shouldBe 123456789123456789U
    }

    @Test fun `should implement toUInt`() {
        JSONDecimal.ZERO.toUInt() shouldBe 0U
        JSONDecimal(12345).toUInt() shouldBe 12345U
        JSONDecimal(2147483648).toUInt() shouldBe 2147483648U
    }

    @Test fun `should implement toUShort`() {
        JSONDecimal.ZERO.toUShort() shouldBe 0U
        JSONDecimal(32768).toUShort() shouldBe 32768U
    }

    @Test fun `should implement toUByte`() {
        JSONDecimal.ZERO.toUByte() shouldBe 0U
        JSONDecimal(129).toUByte() shouldBe 129U
    }

    @Test fun `should format JSONDecimal using output`() {
        val capture = OutputCapture(16)
        JSONDecimal.ZERO.outputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONDecimal(12345).outputTo(capture)
        capture.toString() shouldBe "12345"
        capture.reset()
        JSONDecimal("-527.5").outputTo(capture)
        capture.toString() shouldBe "-527.5"
    }

    @Test fun `should format JSONDecimal using coOutput`() = runBlocking {
        val capture = CoOutputCapture(16)
        JSONDecimal.ZERO.coOutputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONDecimal(12345).coOutputTo(capture)
        capture.toString() shouldBe "12345"
        capture.reset()
        JSONDecimal("-527.5").coOutputTo(capture)
        capture.toString() shouldBe "-527.5"
    }

}
