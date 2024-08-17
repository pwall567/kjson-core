/*
 * @(#) JSONDecimal.kt
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

package io.kjson

import java.math.BigDecimal

/**
 * A JSON decimal value - that is, a JSON number that is not an integer.
 *
 * @author  Peter Wall
 */
class JSONDecimal(override val value: BigDecimal) : JSONNumber(), JSONPrimitive<BigDecimal> {

    constructor(str: String): this(str.toBigDecimal())

    constructor(long: Long): this(long.toBigDecimal())

    constructor(int: Int): this(int.toBigDecimal())

    /**
     * Convert to a JSON string.
     */
    override fun toJSON(): String = if (value == BigDecimal.ZERO) "0" else value.toString()

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) {
        a.append(value.toString())
    }

    /**
     * Return `true` if the value is integral (has no fractional part, or the fractional part is zero).
     */
    override fun isIntegral(): Boolean =
        value.scale() <= 0 || value.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0

    /**
     * Return `true` if the value is will fit in a `Long`.
     */
    override fun isLong(): Boolean = isIntegral() && value in MIN_LONG..MAX_LONG

    /**
     * Return `true` if the value is will fit in an `Int`.
     */
    override fun isInt(): Boolean = isIntegral() && value in MIN_INT..MAX_INT

    /**
     * Return `true` if the value is will fit in a `Short`.
     */
    override fun isShort(): Boolean = isIntegral() && value in MIN_SHORT..MAX_SHORT

    /**
     * Return `true` if the value is will fit in a `Byte`.
     */
    override fun isByte(): Boolean = isIntegral() && value in MIN_BYTE..MAX_BYTE

    /**
     * Return `true` if the value is will fit in a `ULong`.
     */
    override fun isULong(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_ULONG

    /**
     * Return `true` if the value is will fit in a `UInt`.
     */
    override fun isUInt(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_UINT

    /**
     * Return `true` if the value is will fit in a `UShort`.
     */
    override fun isUShort(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_USHORT

    /**
     * Return `true` if the value is will fit in a `UByte`.
     */
    override fun isUByte(): Boolean = isIntegral() && value in BigDecimal.ZERO..MAX_UBYTE

    /**
     * Return `true` if the value is zero.
     */
    override fun isZero(): Boolean = value.compareTo(BigDecimal.ZERO) == 0

    /**
     * Return `true` if the value is negative.
     */
    override fun isNegative(): Boolean = value < BigDecimal.ZERO

    /**
     * Return `true` if the value is positive.
     */
    override fun isPositive(): Boolean = value > BigDecimal.ZERO

    /**
     * Return `true` if the value is not zero.
     */
    override fun isNotZero(): Boolean = value.compareTo(BigDecimal.ZERO) != 0

    /**
     * Return `true` if the value is not negative.
     */
    override fun isNotNegative(): Boolean = value >= BigDecimal.ZERO

    /**
     * Return `true` if the value is not positive.
     */
    override fun isNotPositive(): Boolean = value <= BigDecimal.ZERO

    /**
     * Convert the value to [Double].
     */
    override fun toDouble(): Double = value.toDouble()

    /**
     * Convert the value to [Float].
     */
    override fun toFloat(): Float = value.toFloat()

    /**
     * Convert the value to [Long].
     */
    override fun toLong(): Long = value.toLong()

    /**
     * Convert the value to [Int].
     */
    override fun toInt(): Int = value.toInt()

    /**
     * Convert the value to [Char].
     */
    @Deprecated("Direct conversion to Char is deprecated. Use toInt().toChar() or Char constructor instead.",
            replaceWith = ReplaceWith("this.toInt().toChar()"))
    override fun toChar(): Char = value.toInt().toChar()

    /**
     * Convert the value to [Short].
     */
    override fun toShort(): Short = value.toShort()

    /**
     * Convert the value to [Byte].
     */
    override fun toByte(): Byte = value.toByte()

    /**
     * Convert the value to [BigDecimal].
     */
    override fun toDecimal(): BigDecimal = value

    /**
     * Convert the value to [ULong].
     */
    override fun toULong(): ULong = value.toLong().toULong()

    /**
     * Convert the value to [UInt].
     */
    override fun toUInt(): UInt = value.toInt().toUInt()

    /**
     * Convert the value to [UShort].
     */
    override fun toUShort(): UShort = value.toInt().toUShort()

    /**
     * Convert the value to [UByte].
     */
    override fun toUByte(): UByte = value.toInt().toUByte()

    /**
     * Compare the value to another [JSONNumber] value.  [JSONNumber] objects with different types but the same value
     * are considered equal.
     */
    override fun equals(other: Any?): Boolean = this === other ||
            other is JSONNumber && value.compareTo(other.toDecimal()) == 0

    /**
     * Get the hash code for the [JSONNumber] value.  [JSONNumber] objects with different types but the same value will
     * return the same hash code.
     */
    override fun hashCode(): Int = value.toInt()

    /**
     * Convert the value to [String].
     */
    override fun toString(): String = toJSON()

    /** The value as a [BigDecimal] (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asDecimal: BigDecimal get() = value

    /** The value as a [BigDecimal] or `null` (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asDecimalOrNull: BigDecimal get() = value

    companion object {

        /** A [JSONDecimal] of 0. */
        val ZERO = JSONDecimal(BigDecimal.ZERO)

        private val MIN_LONG = Long.MIN_VALUE.toBigDecimal()
        private val MAX_LONG = Long.MAX_VALUE.toBigDecimal()
        private val MIN_INT = Int.MIN_VALUE.toBigDecimal()
        private val MAX_INT = Int.MAX_VALUE.toBigDecimal()
        private val MIN_SHORT = Short.MIN_VALUE.toInt().toBigDecimal()
        private val MAX_SHORT = Short.MAX_VALUE.toInt().toBigDecimal()
        private val MIN_BYTE = Byte.MIN_VALUE.toInt().toBigDecimal()
        private val MAX_BYTE = Byte.MAX_VALUE.toInt().toBigDecimal()
        private val MAX_ULONG = MAX_LONG + MAX_LONG + BigDecimal.ONE
        private val MAX_UINT = UInt.MAX_VALUE.toLong().toBigDecimal()
        private val MAX_USHORT = UShort.MAX_VALUE.toInt().toBigDecimal()
        private val MAX_UBYTE = UByte.MAX_VALUE.toInt().toBigDecimal()

        /**
         * Create a [JSONDecimal] from a [BigDecimal].
         */
        fun of(d: BigDecimal): JSONDecimal = if (d == BigDecimal.ZERO) ZERO else JSONDecimal(d)

        /**
         * Create a [JSONDecimal] from an [Int].
         */
        fun of(i: Int): JSONDecimal = if (i == 0) ZERO else JSONDecimal(i)

        /**
         * Create a [JSONDecimal] from a [Long].
         */
        fun of(i: Long): JSONDecimal = if (i == 0L) ZERO else JSONDecimal(i)

        /**
         * Create a [JSONDecimal] from a [String].
         */
        fun of(s: String): JSONDecimal = if (s == "0") ZERO else JSONDecimal(s)

    }

}
