/*
 * @(#) ParserConstants.kt
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

import io.kjson.util.LookupSet

/**
 * Constants used by both the main [Parser] and the [`kjson-stream`](https://github.com/pwall567/kjson-stream) library.
 *
 * @author  Peter Wall
 */
@Suppress("unused")
object ParserConstants {

    @Suppress("ConstPropertyName")
    const val rootPointer = ""
    const val BOM = '\uFEFF'
    const val MAX_INTEGER_DIGITS_LENGTH = 10
    const val MAX_LONG_DIGITS_LENGTH = 19

    val identifierStartSet = LookupSet<Char> { it in 'a'..'z' || it in 'A'..'Z' || it == '_' }

    val identifierContinuationSet = LookupSet<Char> { it in 'a'..'z' || it in 'A'..'Z' || it in '0'..'9' || it == '_' }

}
