/*
 * @(#) JSONIncorrectTypeException.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2022 Peter Wall
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

import kotlin.reflect.KClass
import io.kjson.JSON.displayValue

/**
 * Exception class to represent "incorrect type" errors; thrown when a specific type is requested as a return value and
 * the actual value was not of that type.
 *
 * @author  Peter Wall
 */
class JSONIncorrectTypeException(
    val nodeName: String = "Node",
    val target: String,
    val value: JSONValue?,
    val key: Any? = null,
) : JSONException("Incorrect Type") {

    private var lazyMessage: String? = null

    override val message: String
        get() = lazyMessage ?: buildString {
            append(nodeName)
            append(" not correct type (")
            append(target)
            append("), was ")
            append(value.displayValue())
            key?.toString()?.takeIf { it.isNotEmpty() }?.let {
                append(", at ")
                append(it)
            }
        }.also { lazyMessage = it }

}