/*
 * @(#) JSONBoolean.kt
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

/**
 * A JSON boolean value.
 *
 * @author  Peter Wall
 */
class JSONBoolean private constructor(val value: Boolean) : JSONValue {

    override fun toJSON(): String = if (value) "true" else "false"

    override fun appendTo(a: Appendable) {
        a.append(toJSON())
    }

    override fun equals(other: Any?): Boolean = this === other || other is JSONBoolean && value == other.value

    override fun hashCode(): Int = if (value) 1 else 0

    override fun toString(): String = toJSON()

    companion object {

        val TRUE = JSONBoolean(true)
        val FALSE = JSONBoolean(false)

        fun of(b: Boolean): JSONBoolean = if (b) TRUE else FALSE

    }

}
