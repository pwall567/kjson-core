/*
 * @(#) JSONDecimal.kt
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

/**
 * A JSON decimal value - that is, a JSON number that is not an integer.
 *
 * @author  Peter Wall
 */
class JSONDecimal(override val value: BigDecimal) : JSONNumberValue(), JSONPrimitive<BigDecimal> {

    constructor(str: String): this(BigDecimal(str))

    constructor(long: Long): this(BigDecimal(long))

    constructor(int: Int): this(BigDecimal(int))

    override fun toJSON(): String = value.toString()

    override fun appendTo(a: Appendable) {
        a.append(value.toString())
    }

    override fun isIntegral(): Boolean =
        value.scale() <= 0 || value.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0

    override fun isLong(): Boolean = isIntegral() && value in MIN_LONG..MAX_LONG

    override fun isInt(): Boolean = isIntegral() && value in MIN_INT..MAX_INT

    override fun isShort(): Boolean = isIntegral() && value in MIN_SHORT..MAX_SHORT

    override fun isByte(): Boolean = isIntegral() && value in MIN_BYTE..MAX_BYTE

    override fun isULong(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_ULONG

    override fun isUInt(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_UINT

    override fun isUShort(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_USHORT

    override fun isUByte(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_UBYTE

    override fun isZero(): Boolean = value.compareTo(BigDecimal.ZERO) == 0

    override fun isNegative(): Boolean = value < BigDecimal.ZERO

    override fun isPositive(): Boolean = value > BigDecimal.ZERO

    override fun isNotZero(): Boolean = value.compareTo(BigDecimal.ZERO) != 0

    override fun isNotNegative(): Boolean = value >= BigDecimal.ZERO

    override fun isNotPositive(): Boolean = value <= BigDecimal.ZERO

    override fun toDouble(): Double = value.toDouble()

    override fun toFloat(): Float = value.toFloat()

    override fun toLong(): Long = value.toLong()

    override fun toInt(): Int = value.toInt()

    override fun toChar(): Char = value.toChar()

    override fun toShort(): Short = value.toShort()

    override fun toByte(): Byte = value.toByte()

    override fun toDecimal(): BigDecimal = value

    override fun toULong(): ULong = value.toLong().toULong()

    override fun toUInt(): UInt = value.toInt().toUInt()

    override fun toUShort(): UShort = value.toInt().toUShort()

    override fun toUByte(): UByte = value.toInt().toUByte()

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONNumberValue)
            return false
        return when (other) {
            is JSONInt -> value.compareTo(BigDecimal(other.value)) == 0
            is JSONLong -> value.compareTo(BigDecimal(other.value)) == 0
            is JSONDecimal -> value.compareTo(other.value) == 0
        }
    }

    override fun hashCode(): Int = value.toInt()

    override fun toString(): String = toJSON()

    companion object {

        val ZERO = JSONDecimal(BigDecimal.ZERO)

        private val MIN_LONG = BigDecimal(Long.MIN_VALUE)
        private val MAX_LONG = BigDecimal(Long.MAX_VALUE)
        private val MIN_INT = BigDecimal(Int.MIN_VALUE)
        private val MAX_INT = BigDecimal(Int.MAX_VALUE)
        private val MIN_SHORT = BigDecimal(Short.MIN_VALUE.toInt())
        private val MAX_SHORT = BigDecimal(Short.MAX_VALUE.toInt())
        private val MIN_BYTE = BigDecimal(Byte.MIN_VALUE.toInt())
        private val MAX_BYTE = BigDecimal(Byte.MAX_VALUE.toInt())
        private val MAX_ULONG = MAX_LONG * BigDecimal(2) + BigDecimal.ONE
        private val MAX_UINT = BigDecimal(UInt.MAX_VALUE.toLong())
        private val MAX_USHORT = BigDecimal(UShort.MAX_VALUE.toInt())
        private val MAX_UBYTE = BigDecimal(UByte.MAX_VALUE.toInt())

        fun of(d: BigDecimal): JSONDecimal = if (d == BigDecimal.ZERO) ZERO else JSONDecimal(d)

    }

}
