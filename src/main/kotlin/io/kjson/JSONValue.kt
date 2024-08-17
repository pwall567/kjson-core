/*
 * @(#) JSONValue.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2024 Peter Wall
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

import io.kjson.JSON.accept
import net.pwall.util.CoOutput
import net.pwall.util.output

/**
 * Interface to represent a JSON value.
 *
 * @author  Peter Wall
 */
sealed interface JSONValue {

    /**
     * Append as a JSON string to an [Appendable].
     */
    fun appendTo(a: Appendable)

    /**
     * Convert to a JSON string.
     */
    fun toJSON(): String

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    fun outputTo(out: IntConsumer) {
        out.accept(toJSON())
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    @Deprecated("renamed to outputTo", ReplaceWith("outputTo(out)"))
    fun output(out: IntConsumer) {
        out.accept(toJSON())
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    suspend fun coOutputTo(out: CoOutput) {
        out.output(toJSON())
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    @Deprecated("renamed to coOutputTo", ReplaceWith("coOutputTo(out)"))
    suspend fun coOutput(out: CoOutput) {
        out.output(toJSON())
    }

}

/**
 * Construct a [JSONValue] from an [Int].
 */
fun JSONValue(int: Int): JSONValue = JSONInt.of(int)

/**
 * Construct a [JSONValue] from a [Long].
 */
fun JSONValue(long: Long): JSONValue = JSONLong.of(long)

/**
 * Construct a [JSONValue] from a [BigDecimal].
 */
fun JSONValue(decimal: BigDecimal): JSONValue = JSONDecimal.of(decimal)

/**
 * Construct a [JSONValue] from a [String].
 */
fun JSONValue(string: String): JSONValue = JSONString.of(string)

/**
 * Construct a [JSONValue] from a [Boolean].
 */
fun JSONValue(boolean: Boolean): JSONValue = JSONBoolean.of(boolean)

/**
 * Construct a [JSONValue] from an array of [JSONValue]?.
 */
fun JSONValue(vararg items: JSONValue?): JSONValue = JSONArray.of(*items)

/**
 * Construct a [JSONValue] from an array of [Pair]s (of [String] and [JSONValue]?).
 */
fun JSONValue(vararg items: Pair<String, JSONValue?>): JSONValue = JSONObject.of(*items)

/**
 * Construct a [JSONValue] from an array of [JSONObject.Property]s.
 */
fun JSONValue(vararg properties: JSONObject.Property): JSONValue = JSONObject.of(*properties)
