/*
 * @(#) JSONInt.kt
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

import java.math.BigDecimal

import net.pwall.json.JSONFunctions

/**
 * A JSON integer value (up to 32 bits).
 *
 * @author  Peter Wall
 */
class JSONInt(val value: Int) : Number(), JSONValue {

    override fun appendTo(a: Appendable) {
        JSONFunctions.appendInt(a, value)
    }

    override fun toDouble(): Double = value.toDouble()

    override fun toFloat(): Float = value.toFloat()

    override fun toLong(): Long = value.toLong()

    override fun toInt(): Int = value

    override fun toChar(): Char = value.toChar()

    override fun toShort(): Short = value.toShort()

    override fun toByte(): Byte = value.toByte()

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONValue)
            return false
        return when (other) {
            is JSONInt -> value == other.value
            is JSONLong -> value.toLong() == other.value
            is JSONDecimal -> BigDecimal(value).compareTo(other.value) == 0
            else -> false
        }
    }

    override fun hashCode(): Int = value

    override fun toString(): String = value.toString()

    companion object {

        val ZERO = JSONInt(0)

        fun of(i: Int): JSONInt = if (i == 0) ZERO else JSONInt(i)

    }

}
