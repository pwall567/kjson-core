/*
 * @(#) JSONInt.kt
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
import java.util.function.IntConsumer

import net.pwall.util.CoIntOutput.outputInt
import net.pwall.util.CoOutput
import net.pwall.util.IntOutput

/**
 * A JSON integer value (up to 32 bits).
 *
 * @author  Peter Wall
 */
class JSONInt(override val value: Int) : JSONNumberValue(), JSONPrimitive<Int> {

    override fun appendTo(a: Appendable) = IntOutput.appendInt(a, value)

    override fun output(out: IntConsumer) = IntOutput.outputInt(value, out)

    override suspend fun coOutput(out: CoOutput) = out.outputInt(value)

    override fun isIntegral(): Boolean = true

    override fun isLong(): Boolean = true

    override fun isInt(): Boolean = true

    override fun isShort(): Boolean = value in Short.MIN_VALUE..Short.MAX_VALUE

    override fun isByte(): Boolean = value in Byte.MIN_VALUE..Byte.MAX_VALUE

    override fun isULong(): Boolean = value >= 0

    override fun isUInt(): Boolean = value >= 0

    override fun isUShort(): Boolean = value in 0..UShort.MAX_VALUE.toInt()

    override fun isUByte(): Boolean = value in 0..UByte.MAX_VALUE.toInt()

    override fun isZero(): Boolean = value == 0

    override fun isNegative(): Boolean = value < 0

    override fun isPositive(): Boolean = value > 0

    override fun isNotZero(): Boolean = value != 0

    override fun isNotNegative(): Boolean = value >= 0

    override fun isNotPositive(): Boolean = value <= 0

    override fun toDouble(): Double = value.toDouble()

    override fun toFloat(): Float = value.toFloat()

    override fun toLong(): Long = value.toLong()

    override fun toInt(): Int = value

    override fun toChar(): Char = value.toChar()

    override fun toShort(): Short = value.toShort()

    override fun toByte(): Byte = value.toByte()

    override fun toDecimal(): BigDecimal = BigDecimal(value)

    override fun toULong(): ULong = value.toULong()

    override fun toUInt(): UInt = value.toUInt()

    override fun toUShort(): UShort = value.toUShort()

    override fun toUByte(): UByte = value.toUByte()

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        if (other !is JSONNumberValue)
            return false
        return when (other) {
            is JSONInt -> value == other.value
            is JSONLong -> value.toLong() == other.value
            is JSONDecimal -> BigDecimal(value).compareTo(other.value) == 0
        }
    }

    override fun hashCode(): Int = value

    override fun toString(): String = value.toString()

    companion object {

        val ZERO = JSONInt(0)

        fun of(i: Int): JSONInt = if (i == 0) ZERO else JSONInt(i)

    }

}
