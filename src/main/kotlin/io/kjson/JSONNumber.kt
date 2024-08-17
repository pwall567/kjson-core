/*
 * @(#) JSONNumber.kt
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

/**
 * An abstract base class for the three number types of JSON value.  It includes functions to assist with conversion
 * between number types.
 *
 * @author  Peter Wall
 */
sealed class JSONNumber : Number(), JSONValue {

    /** The value (will be overridden by a value of a specific type). */
    abstract val value: Number

    /**
     * Return `true` if the value is integral (has no fractional part, or the fractional part is zero).
     */
    abstract fun isIntegral(): Boolean

    /**
     * Return `true` if the value will fit in a `Long`.
     */
    abstract fun isLong(): Boolean

    /**
     * Return `true` if the value will fit in an `Int`.
     */
    abstract fun isInt(): Boolean

    /**
     * Return `true` if the value will fit in a `Short`.
     */
    abstract fun isShort(): Boolean

    /**
     * Return `true` if the value will fit in a `Byte`.
     */
    abstract fun isByte(): Boolean

    /**
     * Return `true` if the value will fit in a `ULong`.
     */
    abstract fun isULong(): Boolean

    /**
     * Return `true` if the value will fit in a `UInt`.
     */
    abstract fun isUInt(): Boolean

    /**
     * Return `true` if the value will fit in a `UShort`.
     */
    abstract fun isUShort(): Boolean

    /**
     * Return `true` if the value will fit in a `UByte`.
     */
    abstract fun isUByte(): Boolean

    /**
     * Return `true` if the value is zero.
     */
    abstract fun isZero(): Boolean

    /**
     * Return `true` if the value is negative.
     */
    abstract fun isNegative(): Boolean

    /**
     * Return `true` if the value is positive.
     */
    abstract fun isPositive(): Boolean

    /**
     * Return `true` if the value is not zero.
     */
    abstract fun isNotZero(): Boolean

    /**
     * Return `true` if the value is not negative.
     */
    abstract fun isNotNegative(): Boolean

    /**
     * Return `true` if the value is not positive.
     */
    abstract fun isNotPositive(): Boolean

    /**
     * Convert the value to [BigDecimal].
     */
    abstract fun toDecimal(): BigDecimal

    /**
     * Convert the value to [ULong].
     */
    abstract fun toULong(): ULong

    /**
     * Convert the value to [UInt].
     */
    abstract fun toUInt(): UInt

    /**
     * Convert the value to [UShort].
     */
    abstract fun toUShort(): UShort

    /**
     * Convert the value to [UByte].
     */
    abstract fun toUByte(): UByte

    /**
     * Compare the value to another [JSONNumber] value.  [JSONNumber] objects with different types but the same value
     * are considered equal.
     */
    abstract override fun equals(other: Any?): Boolean

    /**
     * Get the hash code for the [JSONNumber] value.  [JSONNumber] objects with different types but the same value will
     * return the same hash code.
     */
    abstract override fun hashCode(): Int

}

/**
 * Construct a [JSONNumber] from an [Int].
 */
fun JSONNumber(int: Int): JSONNumber = JSONInt.of(int)

/**
 * Construct a [JSONNumber] from a [Long].
 */
fun JSONNumber(long: Long): JSONNumber = JSONLong.of(long)

/**
 * Construct a [JSONNumber] from a [BigDecimal].
 */
fun JSONNumber(decimal: BigDecimal): JSONNumber = JSONDecimal.of(decimal)
