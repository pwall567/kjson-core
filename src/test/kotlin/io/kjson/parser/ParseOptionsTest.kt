/*
 * @(#) ParseOptionsTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2023, 2024 Peter Wall
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

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldThrow

class ParseOptionsTest {

    @Test fun `should set maximum nesting depth`() {
        val parseOptions = ParseOptions(maximumNestingDepth = 27)
        parseOptions.maximumNestingDepth shouldBe 27
    }

    @Test fun `should reject maximum nesting depth too low`() {
        shouldThrow<IllegalArgumentException>("Maximum nesting depth must be 1..1200, was 0") {
            ParseOptions(maximumNestingDepth = 0)
        }
        shouldThrow<IllegalArgumentException>("Maximum nesting depth must be 1..1200, was -1") {
            ParseOptions(maximumNestingDepth = -1)
        }
        shouldThrow<IllegalArgumentException>("Maximum nesting depth must be 1..1200, was -10") {
            ParseOptions(maximumNestingDepth = -10)
        }
    }

    @Test fun `should reject maximum nesting depth too high`() {
        shouldThrow<IllegalArgumentException>("Maximum nesting depth must be 1..1200, was 1500") {
            ParseOptions(maximumNestingDepth = 1500)
        }
        shouldThrow<IllegalArgumentException>("Maximum nesting depth must be 1..1200, was 9999") {
            ParseOptions(maximumNestingDepth = 9999)
        }
    }

}
