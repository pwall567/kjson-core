/*
 * @(#) LookupSetTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2023 Peter Wall
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

package io.kjson.util

import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LookupSetTest {

    @Test fun `should perform simple lookup`() {
        assertTrue('a' in LookupSet<Char> { it in 'a'..'z' })
        assertFalse('A' in LookupSet<Char> { it in 'a'..'z' })
        assertTrue("AUD" in LookupSet<String> { it == "AUD" || it == "NZD" })
        assertFalse("EUR" in LookupSet<String> { it == "AUD" || it == "NZD" })
    }

}
