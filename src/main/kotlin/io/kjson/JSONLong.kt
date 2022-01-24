/*
 * @(#) JSONLong.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022 Peter Wall
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

import net.pwall.util.IntOutput.appendLong

/**
 * A JSON long integer value (more than 32 bits but not more than 64 bits).
 *
 * @author  Peter Wall
 */
class JSONLong(override val value: Long) : JSONNumberValue(), JSONValue {

    override fun appendTo(a: Appendable) {
        appendLong(a, value)
    }

    override fun isIntegral(): Boolean = true

    override fun isLong(): Boolean = true

    override fun isInt(): Boolean = value in Int.MIN_VALUE..Int.MAX_VALUE

    override fun isShort(): Boolean = value in Short.MIN_VALUE..Short.MAX_VALUE

    override fun isByte(): Boolean = value in Byte.MIN_VALUE..Byte.MAX_VALUE

    override fun isULong(): Boolean = value >= 0

    override fun isUInt(): Boolean = value in 0..UInt.MAX_VALUE.toLong()

    override fun isUShort(): Boolean = value in 0..UShort.MAX_VALUE.toLong()

    override fun isUByte(): Boolean = value in 0..UByte.MAX_VALUE.toLong()

    override fun isZero(): Boolean = value == 0L

    override fun isNegative(): Boolean = value < 0

    override fun isPositive(): Boolean = value > 0

    override fun isNotZero(): Boolean = value != 0L

    override fun isNotNegative(): Boolean = value >= 0

    override fun isNotPositive(): Boolean = value <= 0

    override fun toDouble(): Double = value.toDouble()

    override fun toFloat(): Float = value.toFloat()

    override fun toLong(): Long = value

    override fun toInt(): Int = value.toInt()

    override fun toChar(): Char = value.toInt().toChar()

    override fun toShort(): Short = value.toShort()

    override fun toByte(): Byte = value.toByte()

    override fun toDecimal(): BigDecimal = BigDecimal(value)

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONValue)
            return false
        return when (other) {
            is JSONInt -> value == other.value.toLong()
            is JSONLong -> value == other.value
            is JSONDecimal -> BigDecimal(value).compareTo(other.value) == 0
            else -> false
        }
    }

    override fun hashCode(): Int = value.toInt()

    override fun toString(): String = value.toString()

    companion object {

        val ZERO = JSONLong(0)

        fun of(i: Long): JSONLong = if (i == 0L) ZERO else JSONLong(i)

    }

}
