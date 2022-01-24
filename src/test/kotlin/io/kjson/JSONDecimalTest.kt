/*
 * @(#) JSONDecimalTest.kt
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
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

class JSONDecimalTest {

    @Test fun `should create JSONDecimal`() {
        JSONDecimal(BigDecimal.ZERO).let {
            expect(BigDecimal.ZERO) { it.value }
            expect("0") { it.toJSON() }
            expect(JSONDecimal("0.00")) { it }
            expect<JSONValue>(JSONInt(0)) { it }
            expect<JSONValue>(JSONLong(0)) { it }
        }
    }

    @Test fun `should create JSONDecimal from Long`() {
        JSONDecimal(123456789123456789).let {
            expect(BigDecimal(123456789123456789)) { it.value }
            expect("123456789123456789") { it.toJSON() }
            expect<JSONValue>(JSONLong(123456789123456789)) { it }
        }
    }

    @Test fun `should create JSONDecimal from Int`() {
        JSONDecimal(12345).let {
            expect(BigDecimal(12345)) { it.value }
            expect("12345") { it.toJSON() }
            expect<JSONValue>(JSONInt(12345)) { it }
        }
    }

    @Test fun `should create JSONDecimal using of`() {
        JSONDecimal.of(BigDecimal.ZERO).let {
            expect(BigDecimal.ZERO) { it.value }
            expect("0") { it.toJSON() }
            expect(JSONDecimal("0.00")) { it }
        }
    }

    @Test fun `should implement isXxxx functions`() {
        JSONDecimal(123456789123456789).let {
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
        JSONDecimal(-123456789123456789).let {
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
        JSONDecimal(123456789).let {
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
        JSONDecimal(-123456789).let {
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
        JSONDecimal(12345).let {
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
        JSONDecimal(-12345).let {
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
        JSONDecimal(123).let {
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
        JSONDecimal(-123).let {
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
        JSONDecimal("0.123").let {
            assertFalse(it.isIntegral())
            assertFalse(it.isLong())
            assertFalse(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONDecimal("-0.123").let {
            assertFalse(it.isIntegral())
            assertFalse(it.isLong())
            assertFalse(it.isInt())
            assertFalse(it.isShort())
            assertFalse(it.isByte())
            assertFalse(it.isULong())
            assertFalse(it.isUInt())
            assertFalse(it.isUShort())
            assertFalse(it.isUByte())
        }
        JSONDecimal("888.00").let {
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
        JSONDecimal("-888.00").let {
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
    }

    @Test fun `should implement isZero etc functions`() {
        JSONDecimal.ZERO.let {
            assertTrue(it.isZero())
            assertFalse(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONDecimal(-123).let {
            assertFalse(it.isZero())
            assertFalse(it.isPositive())
            assertTrue(it.isNegative())
            assertFalse(it.isNotNegative())
            assertTrue(it.isNotPositive())
        }
        JSONDecimal(123).let {
            assertFalse(it.isZero())
            assertTrue(it.isPositive())
            assertFalse(it.isNegative())
            assertTrue(it.isNotNegative())
            assertFalse(it.isNotPositive())
        }
    }

    @Test fun `should implement toDecimal`() {
        expect(BigDecimal.ZERO) { JSONDecimal.ZERO.toDecimal() }
        expect(BigDecimal(12345)) { JSONDecimal(12345).toDecimal() }
        expect(BigDecimal(-9)) { JSONDecimal(-9).toDecimal() }
        expect(BigDecimal("567.89")) { JSONDecimal("567.89").toDecimal() }
    }

    @Test fun `should implement toULong`() {
        expect(0U) { JSONDecimal.ZERO.toULong() }
        expect(123456789123456789U) { JSONDecimal(123456789123456789).toULong() }
    }

    @Test fun `should implement toUInt`() {
        expect(0U) { JSONDecimal.ZERO.toUInt() }
        expect(12345U) { JSONDecimal(12345).toUInt() }
        expect(2147483648U) { JSONDecimal(2147483648).toUInt() }
    }

    @Test fun `should implement toUShort`() {
        expect(0U) { JSONDecimal.ZERO.toUShort() }
        expect(32768U) { JSONDecimal(32768).toUShort() }
    }

    @Test fun `should implement toUByte`() {
        expect(0U) { JSONDecimal.ZERO.toUByte() }
        expect(129U) { JSONDecimal(129).toUByte() }
    }

}
