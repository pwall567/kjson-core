/*
 * @(#) JSONString.kt
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

import java.util.function.IntConsumer

import net.pwall.json.JSONCoFunctions.outputString
import net.pwall.json.JSONFunctions
import net.pwall.util.CoOutput

/**
 * A JSON string value.
 *
 * @author  Peter Wall
 */
class JSONString(override val value: String) : JSONPrimitive<String>, CharSequence {

    override val length = value.length

    override fun get(index: Int): Char = value[index]

    override fun subSequence(startIndex: Int, endIndex: Int) = JSONString(value.substring(startIndex, endIndex))

    override fun appendTo(a: Appendable) = JSONFunctions.appendString(a, value, false)

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun output(out: IntConsumer) = JSONFunctions.outputString(value, false, out)

    /**
     * Output as a JSON string to a [CoOutput].
     */
    override suspend fun coOutput(out: CoOutput) = out.outputString(value, false)

    override fun equals(other: Any?): Boolean = this === other || other is JSONString && value == other.value

    override fun hashCode(): Int = value.hashCode()

    override fun toString(): String = value

    val asString: String
        get() = value

    val asStringOrNull: String
        get() = value

    companion object {

        val EMPTY = JSONString("")

        fun of(s: CharSequence): JSONString = if (s.isEmpty()) EMPTY else JSONString(s.toString())

    }

}
