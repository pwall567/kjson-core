/*
 * @(#) JSONLongTest.kt
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

class JSONLongTest {

    @Test fun `should create a JSONLong`() {
        JSONLong(123).let {
            expect(123L) { it.value }
            expect("123") { it.toJSON() }
            expect("123") { it.toString() }
        }
        JSONLong(-1234567812345678).let {
            expect(-1234567812345678) { it.value }
            expect("-1234567812345678") { it.toJSON() }
            expect("-1234567812345678") { it.toString() }
        }
    }

    @Test fun `should create a JSONLong using of`() {
        JSONLong.of(1122334455667788).let {
            expect(1122334455667788) { it.value }
            expect("1122334455667788") { it.toJSON() }
            expect("1122334455667788") { it.toString() }
        }
        JSONLong.of(-1234567812345678).let {
            expect(-1234567812345678) { it.value }
            expect("-1234567812345678") { it.toJSON() }
            expect("-1234567812345678") { it.toString() }
        }
        assertSame(JSONLong.ZERO, JSONLong.of(0))
    }

    @Test fun `should implement isXxxx functions`() {
        JSONLong(123456789123456789).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertFalse(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertTrue(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONLong(-123456789123456789).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertFalse(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONLong(123456789).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertTrue(it.isULong())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONLong(-123456789).let {
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
        JSONLong(12345).let {
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
        JSONLong(-12345).let {
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
        JSONLong(123).let {
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
        JSONLong(-123).let {
            assertTrue(it.isIntegral())
            assertTrue(it.isLong())
            assertTrue(it.isInt())
            assertTrue(it.isShort())
            assertTrue(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
    }

    @Test fun `should implement isZero etc functions`() {
        JSONLong.ZERO.let {
            assertTrue(it.isZero())
            assertFalse(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONLong(-123).let {
            assertFalse(it.isZero())
            assertFalse(it.isPositive())
            assertTrue(it.isNegative())
            assertFalse(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONLong(123).let {
            assertFalse(it.isZero())
            assertTrue(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertFalse(it.isNotPositive())
        }
    }

    @Test fun `should implement toDecimal`() {
        expect(BigDecimal.ZERO) { JSONLong.ZERO.toDecimal() }
        expect(BigDecimal(123456789123456789)) { JSONLong(123456789123456789).toDecimal() }
        expect(BigDecimal(-9)) { JSONLong(-9).toDecimal() }
    }

}
