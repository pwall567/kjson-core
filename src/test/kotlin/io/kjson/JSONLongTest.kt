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
import kotlin.test.assertFalse
import kotlin.test.assertSame
import kotlin.test.assertTrue
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import java.math.BigDecimal

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

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
        expect(123456789123456789.toBigDecimal()) { JSONLong(123456789123456789).toDecimal() }
        expect((-9).toBigDecimal()) { JSONLong(-9).toDecimal() }
    }

    @Test fun `should implement toULong`() {
        expect(0U) { JSONLong.ZERO.toULong() }
        expect(123456789123456789U) { JSONLong(123456789123456789).toULong() }
    }

    @Test fun `should implement toUInt`() {
        expect(0U) { JSONLong.ZERO.toUInt() }
        expect(12345U) { JSONLong(12345).toUInt() }
        expect(2147483648U) { JSONLong(2147483648).toUInt() }
    }

    @Test fun `should implement toUShort`() {
        expect(0U) { JSONLong.ZERO.toUShort() }
        expect(32768U) { JSONLong(32768).toUShort() }
    }

    @Test fun `should implement toUByte`() {
        expect(0U) { JSONLong.ZERO.toUByte() }
        expect(129U) { JSONLong(129).toUByte() }
    }

    @Test fun `should format JSONLong using output`() {
        val capture = OutputCapture(32)
        JSONLong.ZERO.outputTo(capture)
        expect("0") { capture.toString() }
        capture.reset()
        JSONLong(1234567890123456789).outputTo(capture)
        expect("1234567890123456789") { capture.toString() }
        capture.reset()
        JSONLong(-998877665544332211).outputTo(capture)
        expect("-998877665544332211") { capture.toString() }
    }

    @Test fun `should format JSONLong using coOutput`() = runBlocking {
        val capture = CoOutputCapture(32)
        JSONLong.ZERO.coOutputTo(capture)
        expect("0") { capture.toString() }
        capture.reset()
        JSONLong(1234567890123456789).coOutputTo(capture)
        expect("1234567890123456789") { capture.toString() }
        capture.reset()
        JSONLong(-998877665544332211).coOutputTo(capture)
        expect("-998877665544332211") { capture.toString() }
    }

}
