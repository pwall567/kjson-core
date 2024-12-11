/*
 * @(#) JSONLongTest.kt
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

class JSONLongTest {

    @Test fun `should create a JSONLong`() {
        JSONLong(123).let {
            it.value shouldBe 123L
            it.toJSON() shouldBe "123"
            it.toString() shouldBe "123"
        }
        JSONLong(-1234567812345678).let {
            it.value shouldBe -1234567812345678
            it.toJSON() shouldBe "-1234567812345678"
            it.toString() shouldBe "-1234567812345678"
        }
    }

    @Test fun `should create a JSONLong using of`() {
        JSONLong.of(1122334455667788).let {
            it.value shouldBe 1122334455667788
            it.toJSON() shouldBe "1122334455667788"
            it.toString() shouldBe "1122334455667788"
        }
        JSONLong.of(-1234567812345678).let {
            it.value shouldBe -1234567812345678
            it.toJSON() shouldBe "-1234567812345678"
            it.toString() shouldBe "-1234567812345678"
        }
        JSONLong.of(0) shouldBeSameInstance JSONLong.ZERO
    }

    @Test fun `should implement isXxxx functions`() {
        JSONLong(123456789123456789).let {
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
        JSONLong(-123456789123456789).let {
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
        JSONLong(123456789).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe false
            it.isByte() shouldBe false
            it.isULong() shouldBe true
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
        JSONLong(-123456789).let {
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
        JSONLong(12345).let {
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
        JSONLong(-12345).let {
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
        JSONLong(123).let {
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
        JSONLong(-123).let {
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
    }

    @Test fun `should implement isZero etc functions`() {
        JSONLong.ZERO.let {
            it.isZero() shouldBe true
            it.isPositive() shouldBe false
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe true
        }
        JSONLong(-123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe false
            it.isNegative() shouldBe true
            it.isNotNegative() shouldBe false
            it.isNotPositive() shouldBe true
        }
        JSONLong(123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe true
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe false
        }
    }

    @Test fun `should implement toDecimal`() {
        JSONLong.ZERO.toDecimal() shouldBe BigDecimal.ZERO
        JSONLong(123456789123456789).toDecimal() shouldBe 123456789123456789.toBigDecimal()
        JSONLong(-9).toDecimal() shouldBe (-9).toBigDecimal()
    }

    @Test fun `should implement toULong`() {
        JSONLong.ZERO.toULong() shouldBe 0U
        JSONLong(123456789123456789).toULong() shouldBe 123456789123456789U
    }

    @Test fun `should implement toUInt`() {
        JSONLong.ZERO.toUInt() shouldBe 0U
        JSONLong(12345).toUInt() shouldBe 12345U
        JSONLong(2147483648).toUInt() shouldBe 2147483648U
    }

    @Test fun `should implement toUShort`() {
        JSONLong.ZERO.toUShort() shouldBe 0U
        JSONLong(32768).toUShort() shouldBe 32768U
    }

    @Test fun `should implement toUByte`() {
        JSONLong.ZERO.toUByte() shouldBe 0U
        JSONLong(129).toUByte() shouldBe 129U
    }

    @Test fun `should format JSONLong using output`() {
        val capture = OutputCapture(32)
        JSONLong.ZERO.outputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONLong(1234567890123456789).outputTo(capture)
        capture.toString() shouldBe "1234567890123456789"
        capture.reset()
        JSONLong(-998877665544332211).outputTo(capture)
        capture.toString() shouldBe "-998877665544332211"
    }

    @Test fun `should format JSONLong using coOutput`() = runBlocking {
        val capture = CoOutputCapture(32)
        JSONLong.ZERO.coOutputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONLong(1234567890123456789).coOutputTo(capture)
        capture.toString() shouldBe "1234567890123456789"
        capture.reset()
        JSONLong(-998877665544332211).coOutputTo(capture)
        capture.toString() shouldBe "-998877665544332211"
    }

}
