/*
 * @(#) JSONDecimal.kt
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

/**
 * A JSON decimal value - that is, a JSON number that is not an integer.
 *
 * @author  Peter Wall
 */
class JSONDecimal(override val value: BigDecimal) : JSONNumberValue(), JSONValue {

    constructor(str: String): this(BigDecimal(str))

    constructor(long: Long): this(BigDecimal(long))

    constructor(int: Int): this(BigDecimal(int))

    override fun toJSON(): String = value.toString()

    override fun appendTo(a: Appendable) {
        a.append(value.toString())
    }

    override fun isIntegral(): Boolean =
        value.scale() <= 0 || value.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0

    override fun isLong(): Boolean = isIntegral() && value in BigDecimal(Long.MIN_VALUE)..BigDecimal(Long.MAX_VALUE)

    override fun isInt(): Boolean = isIntegral() && value in BigDecimal(Int.MIN_VALUE)..BigDecimal(Int.MAX_VALUE)

    override fun isShort(): Boolean =
        isIntegral() && value in BigDecimal(Short.MIN_VALUE.toInt())..BigDecimal(Short.MAX_VALUE.toInt())

    override fun isByte(): Boolean =
        isIntegral() && value in BigDecimal(Byte.MIN_VALUE.toInt())..BigDecimal(Byte.MAX_VALUE.toInt())

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

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONValue)
            return false
        return when (other) {
            is JSONInt -> value.compareTo(BigDecimal(other.value)) == 0
            is JSONLong -> value.compareTo(BigDecimal(other.value)) == 0
            is JSONDecimal -> value.compareTo(other.value) == 0
            else -> false
        }
    }

    override fun hashCode(): Int = value.toInt()

    override fun toString(): String = toJSON()

    companion object {

        val ZERO = JSONDecimal(BigDecimal.ZERO)

        fun of(d: BigDecimal): JSONDecimal = if (d == BigDecimal.ZERO) ZERO else JSONDecimal(d)

    }

}
