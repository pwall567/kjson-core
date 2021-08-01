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
import kotlin.test.assertSame
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
        assertSame(test2, JSONInt.ZERO)
    }

}
