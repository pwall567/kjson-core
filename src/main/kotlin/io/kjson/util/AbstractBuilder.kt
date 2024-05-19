/*
 * @(#) AbstractBuilder.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024 Peter Wall
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

import io.kjson.JSONException
import io.kjson.JSONStructure

/**
 * An abstract base class for the `Builder` classes of `JSONArray` and `JSONObject`.
 *
 * @author  Peter Wall
 * @param   T       the type of the array entry
 */
abstract class AbstractBuilder<T>(private var array: Array<T?>?) {

    private var count: Int = 0

    val size: Int
        get() = count

    fun checkArray() = array ?: throw JSONException("Builder is closed")

    protected fun internalAdd(value: T?) {
        var validArray = checkArray()
        val len = validArray.size
        if (count >= len) {
            validArray = validArray.copyOf(len + len.coerceAtMost(4096))
            array = validArray
        }
        validArray[count++] = value
    }

    protected fun internalRemove(index: Int) {
        val validArray = checkArray()
        System.arraycopy(validArray, index + 1, validArray, index, count - index)
        count--
    }

    protected fun invalidate() {
        array = null
    }

    abstract fun build(): JSONStructure<*>

}
