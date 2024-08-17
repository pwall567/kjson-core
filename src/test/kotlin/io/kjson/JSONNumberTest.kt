/*
 * @(#) JSONNumberTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2024 Peter Wall
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
import kotlin.test.assertIs
import kotlin.test.assertSame
import kotlin.test.expect

import java.math.BigDecimal

class JSONNumberTest {

    @Test fun `should create JSONNumber of correct type using JSONNumber function`() {
        JSONNumber(123).let {
            assertIs<JSONInt>(it)
            expect(123) { it.value }
        }
        JSONNumber(1234567890123456789).let {
            assertIs<JSONLong>(it)
            expect(1234567890123456789) { it.value }
        }
        JSONNumber(BigDecimal.ONE).let {
            assertIs<JSONDecimal>(it)
            expect(BigDecimal.ONE) { it.value }
        }
        assertSame(JSONInt.ZERO, JSONNumber(0))
        assertSame(JSONLong.ZERO, JSONNumber(0L))
        assertSame(JSONDecimal.ZERO, JSONNumber(BigDecimal.ZERO))
    }

}
