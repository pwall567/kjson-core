/*
 * @(#) JSONIntTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022 Peter Wall
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
        JSONInt(12345).let {
            expect(12345) { it.value }
            expect("12345") { it.toJSON() }
            expect("12345") { it.toString() }
            expect<JSONValue>(JSONLong(12345)) { it }
            expect<JSONValue>(JSONDecimal(BigDecimal(12345))) { it }
        }
        JSONInt(-888).let {
            expect(-888) { it.value }
            expect("-888") { it.toJSON() }
            expect("-888") { it.toString() }
        }
    }

    @Test fun `should create JSONInt using of`() {
        JSONInt.of(12345).let {
            expect(12345) { it.value }
            expect("12345") { it.toJSON() }
            expect("12345") { it.toString() }
        }
        assertSame(JSONInt.ZERO, JSONInt.of(0))
    }

    @Test fun `should implement isXxxx functions`() {
        JSONInt(123456789).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertTrue(it.isULong())
            assertTrue(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONInt(-123456789).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONInt(12345).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertTrue(it.isShort())
            assertFalse(it.isByte())
            assertTrue(it.isULong())
            assertTrue(it.isUInt())
            assertTrue(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONInt(-12345).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertTrue(it.isShort())
            assertFalse(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONInt(123).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertTrue(it.isShort())
            assertTrue(it.isByte())
            assertTrue(it.isULong())
            assertTrue(it.isUInt())
            assertTrue(it.isUShort())
            assertTrue(it.isUByte())
        }
        JSONInt(-123).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertTrue(it.isShort())
            assertTrue(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
    }

    @Test fun `should implement isZero etc functions`() {
        JSONInt.ZERO.let {
            assertTrue(it.isZero())
            assertFalse(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONInt(-123).let {
            assertFalse(it.isZero())
            assertFalse(it.isPositive())
            assertTrue(it.isNegative())
            assertFalse(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONInt(123).let {
            assertFalse(it.isZero())
            assertTrue(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertFalse(it.isNotPositive())
        }
    }

}
