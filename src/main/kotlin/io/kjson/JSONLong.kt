/*
 * @(#) JSONLong.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023 Peter Wall
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
import java.util.function.IntConsumer

import net.pwall.util.CoIntOutput.outputLong
import net.pwall.util.CoOutput
import net.pwall.util.IntOutput

/**
 * A JSON long integer value (more than 32 bits but not more than 64 bits).
 *
 * @author  Peter Wall
 */
class JSONLong(override val value: Long) : JSONNumber(), JSONPrimitive<Long> {

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) = IntOutput.appendLong(a, value)

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun output(out: IntConsumer) = IntOutput.outputLong(value, out)

    /**
     * Output as a JSON string to a [CoOutput].
     */
    override suspend fun coOutput(out: CoOutput) = out.outputLong(value)

    /**
     * Return `true` if the value is integral (has no fractional part, or the fractional part is zero).
     */
    override fun isIntegral(): Boolean = true

    /**
     * Return `true` if the value is will fit in a `Long`.
     */
    override fun isLong(): Boolean = true

    /**
     * Return `true` if the value is will fit in an `Int`.
     */
    override fun isInt(): Boolean = value in Int.MIN_VALUE..Int.MAX_VALUE

    /**
     * Return `true` if the value is will fit in a `Short`.
     */
    override fun isShort(): Boolean = value in Short.MIN_VALUE..Short.MAX_VALUE

    /**
     * Return `true` if the value is will fit in a `Byte`.
     */
    override fun isByte(): Boolean = value in Byte.MIN_VALUE..Byte.MAX_VALUE

    /**
     * Return `true` if the value is will fit in a `ULong`.
     */
    override fun isULong(): Boolean = value >= 0

    /**
     * Return `true` if the value is will fit in a `UInt`.
     */
    override fun isUInt(): Boolean = value in 0..UInt.MAX_VALUE.toLong()

    /**
     * Return `true` if the value is will fit in a `UShort`.
     */
    override fun isUShort(): Boolean = value in 0..UShort.MAX_VALUE.toLong()

    /**
     * Return `true` if the value is will fit in a `UByte`.
     */
    override fun isUByte(): Boolean = value in 0..UByte.MAX_VALUE.toLong()

    /**
     * Return `true` if the value is zero.
     */
    override fun isZero(): Boolean = value == 0L

    /**
     * Return `true` if the value is negative.
     */
    override fun isNegative(): Boolean = value < 0

    /**
     * Return `true` if the value is positive.
     */
    override fun isPositive(): Boolean = value > 0

    /**
     * Return `true` if the value is not zero.
     */
    override fun isNotZero(): Boolean = value != 0L

    /**
     * Return `true` if the value is not negative.
     */
    override fun isNotNegative(): Boolean = value >= 0

    /**
     * Return `true` if the value is not positive.
     */
    override fun isNotPositive(): Boolean = value <= 0

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
    override fun toLong(): Long = value

    /**
     * Convert the value to [Int].
     */
    override fun toInt(): Int = value.toInt()

    /**
     * Convert the value to [Char].
     */
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
    override fun toDecimal(): BigDecimal = BigDecimal(value)

    /**
     * Convert the value to [ULong].
     */
    override fun toULong(): ULong = value.toULong()

    /**
     * Convert the value to [UInt].
     */
    override fun toUInt(): UInt = value.toUInt()

    /**
     * Convert the value to [UShort].
     */
    override fun toUShort(): UShort = value.toUShort()

    /**
     * Convert the value to [UByte].
     */
    override fun toUByte(): UByte = value.toUByte()

    /**
     * Compare the value to another [JSONNumber] value.  [JSONNumber] objects with different types but the same value
     * are considered equal.
     */
    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONNumber)
            return false
        return when (other) {
            is JSONInt -> value == other.value.toLong()
            is JSONLong -> value == other.value
            is JSONDecimal -> BigDecimal(value).compareTo(other.value) == 0
        }
    }

    /**
     * Get the hash code for the [JSONNumber] value.  [JSONNumber] objects with different types but the same value will
     * return the same hash code.
     */
    override fun hashCode(): Int = value.toInt()

    /**
     * Convert the value to [String].
     */
    override fun toString(): String = value.toString()

    /** The value as a [Long] (optimisation of the extension value in [JSON] when the type is known statically). */
    val asLong: Long
        get() = value

    /** The value as a [Long] or `null` (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asLongOrNull: Long
        get() = value

    /** The value as a [Long] (optimisation of the extension value in [JSON] when the type is known statically). */
    val asDecimal: BigDecimal
        get() = BigDecimal(value)

    /** The value as a [Long] or `null` (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asDecimalOrNull: BigDecimal
        get() = BigDecimal(value)

    companion object {

        /** A [JSONLong] of 0. */
        val ZERO = JSONLong(0)

        /**
         * Create a [JSONLong] from a [Long].
         */
        fun of(i: Long): JSONLong = if (i == 0L) ZERO else JSONLong(i)

        /**
         * Create a [JSONLong] from an [Int].
         */
        fun of(i: Int): JSONLong = if (i == 0) ZERO else JSONLong(i.toLong())

    }

}
