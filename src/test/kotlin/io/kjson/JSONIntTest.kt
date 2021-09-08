/*
 * @(#) JSONIntTest.kt
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

package io.kjson

import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertSame
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

class JSONIntTest {

    @Test fun `should create JSONInt`() {
        val test1 = JSONInt(12345)
        expect(12345) { test1.value }
        expect("12345") { test1.toJSON() }
        expect("12345") { test1.toString() }
        expect<JSONValue>(JSONLong(12345)) { test1 }
        expect<JSONValue>(JSONDecimal(BigDecimal(12345))) { test1 }
        val test2 = JSONInt(-888)
        expect(-888) { test2.value }
        expect("-888") { test2.toJSON() }
        expect("-888") { test2.toString() }
    }

    @Test fun `should create JSONInt using of`() {
        val test1 = JSONInt.of(12345)
        expect(12345) { test1.value }
        expect("12345") { test1.toJSON() }
        expect("12345") { test1.toString() }
        val test2 = JSONInt.of(0)
        assertSame(JSONInt.ZERO, test2)
    }

    @Test fun `should implement isXxxx functions`() {
        val test1 = JSONInt(123456789)
        assertTrue(test1.isIntegral())
        assertTrue(test1.isLong())
        assertTrue(test1.isInt())
        assertFalse(test1.isShort())
        assertFalse(test1.isByte())
        val test2 = JSONInt(12345)
        assertTrue(test2.isIntegral())
        assertTrue(test2.isLong())
        assertTrue(test2.isInt())
        assertTrue(test2.isShort())
        assertFalse(test2.isByte())
        val test3 = JSONInt(123)
        assertTrue(test3.isIntegral())
        assertTrue(test3.isLong())
        assertTrue(test3.isInt())
        assertTrue(test3.isShort())
        assertTrue(test3.isByte())
    }

    @Test fun `should implement isZero etc functions`() {
        val test1 = JSONInt.ZERO
        assertTrue(test1.isZero())
        assertFalse(test1.isPositive())
        assertFalse(test1.isNegative())
        assertTrue(test1.isNotNegative())
        assertTrue(test1.isNotPositive())
        val test2 = JSONInt(-123)
        assertFalse(test2.isZero())
        assertFalse(test2.isPositive())
        assertTrue(test2.isNegative())
        assertFalse(test2.isNotNegative())
        assertTrue(test2.isNotPositive())
        val test3 = JSONInt(123)
        assertFalse(test3.isZero())
        assertTrue(test3.isPositive())
        assertFalse(test3.isNegative())
        assertTrue(test3.isNotNegative())
        assertFalse(test3.isNotPositive())
    }

}
