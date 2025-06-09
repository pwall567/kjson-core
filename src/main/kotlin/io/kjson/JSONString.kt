/*
 * @(#) JSONString.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024, 2025 Peter Wall
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

import io.jstuff.json.JSONFunctions
import io.kstuff.json.JSONCoFunctions.outputString
import io.kstuff.util.CoOutput

/**
 * A JSON string value.
 *
 * @author  Peter Wall
 */
class JSONString(override val value: String) : JSONPrimitive<String>, CharSequence {

    // Implementation Note: this class seemed a good candidate for a value class, but the attempt to convert it to a
    // value class (with Kotlin 2.0.21) ran into problems; specifically, the function names in the companion object were
    // generated with "mangled" names (see https://kotlinlang.org/docs/inline-classes.html#mangling), but later uses of
    // the library generated code that called the un-mangled forms, resulting in java.lang.NoSuchMethodError exceptions.
    // It may be worth trying again with a later version of Kotlin, but the performance gains may be negligible.

    /** The length of the string */
    override val length: Int get() = value.length

    /**
     * Get a single [Char] from the string.
     */
    override fun get(index: Int): Char = value[index]

    /**
     * Create a new [JSONString] from a sub-sequence of the value, using the specified start and end indices.
     */
    override fun subSequence(startIndex: Int, endIndex: Int) = JSONString(value.substring(startIndex, endIndex))

    /**
     * Convert to a JSON string.
     */
    override fun toJSON(): String = if (value.isEmpty())
        "\"\""
    else
        buildString(((value.length * 9) shr 3) + 2) { // 12.5% extra for escape sequences, plus 2 for quotes
            appendTo(this)
        }

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) {
        JSONFunctions.appendString(a, value, false)
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun outputTo(out: IntConsumer) {
        JSONFunctions.outputString(value, false, out)
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    @Deprecated("renamed to outputTo", ReplaceWith("outputTo(out)"))
    override fun output(out: IntConsumer) {
        JSONFunctions.outputString(value, false, out)
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    override suspend fun coOutputTo(out: CoOutput) {
        out.outputString(value, false)
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    @Deprecated("renamed to coOutputTo", ReplaceWith("coOutputTo(out)"))
    override suspend fun coOutput(out: CoOutput) {
        out.outputString(value, false)
    }

    /**
     * Compare the value to another value.
     */
    override fun equals(other: Any?): Boolean = this === other || other is JSONString && value == other.value

    /**
     * Get the hash code for the [JSONString] value.
     */
    override fun hashCode(): Int = value.hashCode()

    /**
     * Get the string value.
     */
    override fun toString(): String = value

    /** The value as a [String] (optimisation of the extension value in [JSON] when the type is known statically). */
    val asString: String get() = value

    /** The value as a [String] or `null` (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asStringOrNull: String get() = value

    companion object {

        /** An empty [String]. */
        const val EMPTY_STRING = ""

        /** An empty [JSONString]. */
        val EMPTY = JSONString(EMPTY_STRING)

        /**
         * Create a [JSONString] from the given [CharSequence].
         */
        fun of(s: CharSequence): JSONString = if (s.isEmpty()) EMPTY else JSONString(s.toString())

        /**
         * Create a [JSONString] by appending to a [StringBuilder].
         */
        fun build(block: StringBuilder.() -> Unit): JSONString = of(StringBuilder().apply(block))

    }

}
