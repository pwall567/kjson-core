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
        val test2 = JSONLong(-1234567812345678)
        expect(-1234567812345678) { test2.value }
        expect("-1234567812345678") { test2.toJSON() }
        expect("-1234567812345678") { test2.toString() }
    }

}
