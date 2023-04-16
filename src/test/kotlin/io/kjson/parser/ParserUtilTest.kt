/*
 * @(#) ParserUtilTest.kt
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
import kotlin.test.assertFalse
import kotlin.test.assertTrue

import io.kjson.parser.ParserConstants.identifierContinuationSet
import io.kjson.parser.ParserConstants.identifierStartSet

class ParserUtilTest {

    @Test fun `should recognise leading character of identifier`() {
        assertTrue('A' in identifierStartSet)
        assertTrue('B' in identifierStartSet)
        assertTrue('Z' in identifierStartSet)
        assertTrue('a' in identifierStartSet)
        assertTrue('z' in identifierStartSet)
        assertTrue('_' in identifierStartSet)
        assertFalse('0' in identifierStartSet)
        assertFalse('9' in identifierStartSet)
        assertFalse('.' in identifierStartSet)
        assertFalse('"' in identifierStartSet)
        assertFalse('{' in identifierStartSet)
        assertFalse('[' in identifierStartSet)
    }

    @Test fun `should recognise continuation character of identifier`() {
        assertTrue('A' in identifierContinuationSet)
        assertTrue('B' in identifierContinuationSet)
        assertTrue('Z' in identifierContinuationSet)
        assertTrue('a' in identifierContinuationSet)
        assertTrue('z' in identifierContinuationSet)
        assertTrue('_' in identifierContinuationSet)
        assertTrue('0' in identifierContinuationSet)
        assertTrue('9' in identifierContinuationSet)
        assertFalse('.' in identifierContinuationSet)
        assertFalse('"' in identifierContinuationSet)
        assertFalse('{' in identifierContinuationSet)
        assertFalse('[' in identifierContinuationSet)
    }

}
