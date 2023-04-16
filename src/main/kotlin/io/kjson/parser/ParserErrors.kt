/*
 * @(#) ParserErrors.kt
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

/**
 * Error messages used by both the main [Parser] and the [`kjson-stream`](https://github.com/pwall567/kjson-stream)
 * library.
 *
 * @author  Peter Wall
 */
@Suppress("unused")
object ParserErrors {

    const val EXCESS_CHARS = "Excess characters following JSON"
    const val ILLEGAL_NUMBER = "Illegal JSON number"
    const val ILLEGAL_LEADING_ZERO = "Illegal leading zero in JSON number"
    const val ILLEGAL_SYNTAX = "Illegal JSON syntax"
    const val ILLEGAL_KEY = "Illegal key in JSON object"
    const val DUPLICATE_KEY = "Duplicate key in JSON object"
    const val MISSING_COLON = "Missing colon in JSON object"
    const val MISSING_COMMA_ARRAY = "Missing comma in JSON array"
    const val MISSING_COMMA_OBJECT = "Missing comma in JSON object"
    const val MISSING_CLOSING_BRACE = "Missing closing brace in JSON object"
    const val MISSING_CLOSING_BRACKET = "Missing closing bracket in JSON array"
    const val JSON_INCOMPLETE = "JSON is incomplete"
    const val ARRAY_INCOMPLETE = "JSON array is incomplete"
    const val OBJECT_INCOMPLETE = "JSON object is incomplete"
    const val NUMBER_INCOMPLETE = "JSON number is incomplete"
    const val KEYWORD_INCOMPLETE = "JSON keyword is incomplete"
    const val INVALID_KEYWORD = "Unrecognised JSON keyword"
    const val TRAILING_COMMA_ARRAY = "Trailing comma in JSON array"
    const val TRAILING_COMMA_OBJECT = "Trailing comma in JSON object"
    const val ILLEGAL_ARRAY = "Array must start with open bracket"

}
