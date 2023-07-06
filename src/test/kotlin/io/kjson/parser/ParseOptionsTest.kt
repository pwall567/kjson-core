/*
 * @(#) ParseOptionsTest.kt
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

package io.kjson.parser

import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.expect

class ParseOptionsTest {

    @Test fun `should set maximum nesting depth`() {
        val parseOptions = ParseOptions(maximumNestingDepth = 27)
        expect(27) { parseOptions.maximumNestingDepth }
    }

    @Test fun `should reject maximum nesting depth too low`() {
        assertFailsWith<IllegalArgumentException> { ParseOptions(maximumNestingDepth = 0) }.let {
            expect("Maximum nesting depth must be 1..1200, was 0") { it.message }
        }
        assertFailsWith<IllegalArgumentException> { ParseOptions(maximumNestingDepth = -1) }.let {
            expect("Maximum nesting depth must be 1..1200, was -1") { it.message }
        }
        assertFailsWith<IllegalArgumentException> { ParseOptions(maximumNestingDepth = -10) }.let {
            expect("Maximum nesting depth must be 1..1200, was -10") { it.message }
        }
    }

    @Test fun `should reject maximum nesting depth too high`() {
        assertFailsWith<IllegalArgumentException> { ParseOptions(maximumNestingDepth = 1500) }.let {
            expect("Maximum nesting depth must be 1..1200, was 1500") { it.message }
        }
        assertFailsWith<IllegalArgumentException> { ParseOptions(maximumNestingDepth = 9999) }.let {
            expect("Maximum nesting depth must be 1..1200, was 9999") { it.message }
        }
    }

}
