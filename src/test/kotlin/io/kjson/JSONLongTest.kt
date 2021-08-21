/*
 * @(#) JSONLongTest.kt
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

class JSONLongTest {

    @Test fun `should create a JSONLong`() {
        val test1 = JSONLong(123)
        expect(123L) { test1.value }
        expect("123") { test1.toJSON() }
        expect("123") { test1.toString() }
        val test2 = JSONLong(-1234567812345678)
        expect(-1234567812345678) { test2.value }
        expect("-1234567812345678") { test2.toJSON() }
        expect("-1234567812345678") { test2.toString() }
    }

    @Test fun `should create a JSONLong using of`() {
        val test1 = JSONLong.of(1122334455667788)
        expect(1122334455667788) { test1.value }
        expect("1122334455667788") { test1.toJSON() }
        expect("1122334455667788") { test1.toString() }
        val test2 = JSONLong.of(-1234567812345678)
        expect(-1234567812345678) { test2.value }
        expect("-1234567812345678") { test2.toJSON() }
        expect("-1234567812345678") { test2.toString() }
        val test3 = JSONLong.of(0)
        assertSame(JSONLong.ZERO, test3)
    }

    @Test fun `should implement isXxxx functions`() {
        val test0 = JSONLong(123456789123456789)
        assertTrue(test0.isIntegral())
        assertTrue(test0.isLong())
        assertFalse(test0.isInt())
        assertFalse(test0.isShort())
        assertFalse(test0.isByte())
        val test1 = JSONLong(123456789)
        assertTrue(test1.isIntegral())
        assertTrue(test1.isLong())
        assertTrue(test1.isInt())
        assertFalse(test1.isShort())
        assertFalse(test1.isByte())
        val test2 = JSONLong(12345)
        assertTrue(test2.isIntegral())
        assertTrue(test2.isLong())
        assertTrue(test2.isInt())
        assertTrue(test2.isShort())
        assertFalse(test2.isByte())
        val test3 = JSONLong(123)
        assertTrue(test3.isIntegral())
        assertTrue(test3.isLong())
        assertTrue(test3.isInt())
        assertTrue(test3.isShort())
        assertTrue(test3.isByte())
    }

}
