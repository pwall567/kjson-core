/*
 * @(#) JSONDecimalTest.kt
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
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

class JSONDecimalTest {

    @Test fun `should create JSONDecimal`() {
        val testDecimal1 = JSONDecimal(BigDecimal.ZERO)
        expect(BigDecimal.ZERO) { testDecimal1.value }
        expect("0") { testDecimal1.toJSON() }
        expect(JSONDecimal("0.00")) { testDecimal1 }
        expect<JSONValue>(JSONInt(0)) { testDecimal1 }
        expect<JSONValue>(JSONLong(0)) { testDecimal1 }
    }

    @Test fun `should create JSONDecimal from Long`() {
        val testDecimal1 = JSONDecimal(123456789123456789)
        expect(BigDecimal(123456789123456789)) { testDecimal1.value }
        expect("123456789123456789") { testDecimal1.toJSON() }
        expect<JSONValue>(JSONLong(123456789123456789)) { testDecimal1 }
    }

    @Test fun `should create JSONDecimal from Int`() {
        val testDecimal1 = JSONDecimal(12345)
        expect(BigDecimal(12345)) { testDecimal1.value }
        expect("12345") { testDecimal1.toJSON() }
        expect<JSONValue>(JSONInt(12345)) { testDecimal1 }
    }

    @Test fun `should create JSONDecimal using of`() {
        val testDecimal1 = JSONDecimal.of(BigDecimal.ZERO)
        expect(BigDecimal.ZERO) { testDecimal1.value }
        expect("0") { testDecimal1.toJSON() }
        expect(JSONDecimal("0.00")) { testDecimal1 }
    }

    @Test fun `should implement isXxxx functions`() {
        val test0 = JSONDecimal(123456789123456789)
        assertTrue(test0.isIntegral())
        assertTrue(test0.isLong())
        assertFalse(test0.isInt())
        assertFalse(test0.isShort())
        assertFalse(test0.isByte())
        val test1 = JSONDecimal(123456789)
        assertTrue(test1.isIntegral())
        assertTrue(test1.isLong())
        assertTrue(test1.isInt())
        assertFalse(test1.isShort())
        assertFalse(test1.isByte())
        val test2 = JSONDecimal(12345)
        assertTrue(test2.isIntegral())
        assertTrue(test2.isLong())
        assertTrue(test2.isInt())
        assertTrue(test2.isShort())
        assertFalse(test2.isByte())
        val test3 = JSONDecimal(123)
        assertTrue(test3.isIntegral())
        assertTrue(test3.isLong())
        assertTrue(test3.isInt())
        assertTrue(test3.isShort())
        assertTrue(test3.isByte())
        val test4 = JSONDecimal("0.123")
        assertFalse(test4.isIntegral())
        assertFalse(test4.isLong())
        assertFalse(test4.isInt())
        assertFalse(test4.isShort())
        assertFalse(test4.isByte())
        val test5 = JSONDecimal("888.00")
        assertTrue(test5.isIntegral())
        assertTrue(test5.isLong())
        assertTrue(test5.isInt())
        assertTrue(test5.isShort())
        assertFalse(test5.isByte())
    }

}
