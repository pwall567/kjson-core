/*
 * @(#) JSONNumberTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2024, 2025 Peter Wall
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

import java.math.BigDecimal

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeSameInstance
import io.kstuff.test.shouldBeType

class JSONNumberTest {

    @Test fun `should create JSONInt using JSONNumber function`() {
        JSONNumber(123).let {
            it.shouldBeType<JSONInt>()
            it.value shouldBe 123
        }
        JSONNumber(3L).let {
            it.shouldBeType<JSONInt>()
            it.value shouldBe 3
        }
        JSONNumber(BigDecimal.ONE).let {
            it.shouldBeType<JSONInt>()
            it.value shouldBe 1
        }
        JSONNumber(0) shouldBeSameInstance JSONInt.ZERO
    }

    @Test fun `should create JSONLong using JSONNumber function`() {
        JSONNumber(1234567890123456789).let {
            it.shouldBeType<JSONLong>()
            it.value shouldBe 1234567890123456789
        }
        JSONNumber(BigDecimal("1234567890123456789")).let {
            it.shouldBeType<JSONLong>()
            it.value shouldBe 1234567890123456789
        }
    }

    @Test fun `should create JSONDecimal using JSONNumber function`() {
        JSONNumber(BigDecimal("1.5")).let {
            it.shouldBeType<JSONDecimal>()
            it.value shouldBe BigDecimal("1.5")
        }
    }

}
