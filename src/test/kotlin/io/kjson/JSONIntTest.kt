/*
 * @(#) JSONIntTest.kt
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

class JSONIntTest {

    @Test fun `should create JSONInt`() {
        JSONInt(12345).let {
            it.value shouldBe 12345
            it.toJSON() shouldBe "12345"
            it.toString() shouldBe "12345"
            it.shouldBe<JSONValue>(JSONLong(12345))
            it.shouldBe<JSONValue>(JSONDecimal(12345.toBigDecimal()))
        }
        JSONInt(-888).let {
            it.value shouldBe -888
            it.toJSON() shouldBe "-888"
            it.toString() shouldBe "-888"
        }
    }

    @Test fun `should create JSONInt using of`() {
        JSONInt.of(12345).let {
            it.value shouldBe 12345
            it.toJSON() shouldBe "12345"
            it.toString() shouldBe "12345"
        }
        JSONInt.of(0) shouldBeSameInstance JSONInt.ZERO
    }

    @Test fun `should implement isXxxx functions`() {
        JSONInt(123456789).let {
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
        JSONInt(-123456789).let {
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
        JSONInt(12345).let {
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
        JSONInt(-12345).let {
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
        JSONInt(123).let {
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
        JSONInt(-123).let {
            it.isIntegral() shouldBe true
            it.isLong() shouldBe true
            it.isInt() shouldBe true
            it.isShort() shouldBe true
            it.isByte() shouldBe true
            it.isULong() shouldBe false
            it.isUShort() shouldBe false
            it.isUByte() shouldBe false
        }
    }

    @Test fun `should implement isZero etc functions`() {
        JSONInt.ZERO.let {
            it.isZero() shouldBe true
            it.isPositive() shouldBe false
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe true
        }
        JSONInt(-123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe false
            it.isNegative() shouldBe true
            it.isNotNegative() shouldBe false
            it.isNotPositive() shouldBe true
        }
        JSONInt(123).let {
            it.isZero() shouldBe false
            it.isPositive() shouldBe true
            it.isNegative() shouldBe false
            it.isNotNegative() shouldBe true
            it.isNotPositive() shouldBe false
        }
    }

    @Test fun `should implement toDecimal`() {
        JSONInt.ZERO.toDecimal() shouldBe BigDecimal.ZERO
        JSONInt(12345).toDecimal() shouldBe 12345.toBigDecimal()
        JSONInt(-9).toDecimal() shouldBe (-9).toBigDecimal()
    }

    @Test fun `should implement toULong`() {
        JSONInt.ZERO.toULong() shouldBe 0U
        JSONInt(12345).toULong() shouldBe 12345U
    }

    @Test fun `should implement toUInt`() {
        JSONInt.ZERO.toUInt() shouldBe 0U
        JSONInt(12345).toUInt() shouldBe 12345U
    }

    @Test fun `should implement toUShort`() {
        JSONInt.ZERO.toUShort() shouldBe 0U
        JSONInt(32768).toUShort() shouldBe 32768U
    }

    @Test fun `should implement toUByte`() {
        JSONInt.ZERO.toUByte() shouldBe 0U
        JSONInt(129).toUByte() shouldBe 129U
    }

    @Test fun `should format JSONInt using output`() {
        val capture = OutputCapture(16)
        JSONInt.ZERO.outputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONInt(1234567).outputTo(capture)
        capture.toString() shouldBe "1234567"
        capture.reset()
        JSONInt(-888).outputTo(capture)
        capture.toString() shouldBe "-888"
    }

    @Test fun `should format JSONInt using coOutput`() = runBlocking {
        val capture = CoOutputCapture(16)
        JSONInt.ZERO.coOutputTo(capture)
        capture.toString() shouldBe "0"
        capture.reset()
        JSONInt(1234567).coOutputTo(capture)
        capture.toString() shouldBe "1234567"
        capture.reset()
        JSONInt(-888).coOutputTo(capture)
        capture.toString() shouldBe "-888"
    }

}
