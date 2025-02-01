/*
 * @(#) JSONArray.kt
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
import java.util.function.IntConsumer

import io.jstuff.util.ImmutableCollection
import io.jstuff.util.ImmutableList
import io.kstuff.util.CoOutput
import io.kstuff.util.CoOutputFlushable
import io.kstuff.util.output

import io.kjson.JSON.appendTo
import io.kjson.JSON.coOutputTo
import io.kjson.JSON.outputTo
import io.kjson.util.AbstractBuilder

/**
 * A JSON array.  As allowed by the JSON specification, array members may be primitive types, objects, other arrays or
 * `null`.
 *
 * A `JSONArray` is immutable; instances may be created dynamically using the [build] function and the [Builder] class.
 *
 * The `JSONArray` class implements the Kotlin [List] interface; all the functions of that interface are available,
 * including indexed access, iteration over the list contents and comparison with another list.
 *
 * @author  Peter Wall
 */
class JSONArray internal constructor (private val array: Array<out JSONValue?>, override val size: Int) :
        JSONStructure<Int>, List<JSONValue?> by ImmutableList<JSONValue?>(array, size) {

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) {
        a.append('[')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                array[i].appendTo(a)
                if (++i >= size)
                    break
                a.append(',')
            }
        }
        a.append(']')
    }

    /**
     * Append in JSON Lines form to an [Appendable].
     */
    fun appendJSONLinesTo(a: Appendable) {
        var i = 0
        while (i < size) {
            array[i++].appendTo(a)
            a.append('\n')
        }
    }

    /**
     * Append in JSON Lines form to an [Appendable].
     */
    @Deprecated("renamed to appendJSONLinesTo", ReplaceWith("appendJSONLinesTo(a)"))
    fun appendJSONLines(a: Appendable) {
        var i = 0
        while (i < size) {
            array[i++].appendTo(a)
            a.append('\n')
        }
    }

    /**
     * Convert to a JSON string.
     */
    override fun toJSON(): String = if (isEmpty())
        "[]"
    else
        buildString(JSON.defaultOutputBuilderSize) {
            appendTo(this)
        }

    /**
     * Convert to a string in JSON Lines form.
     */
    fun toJSONLines(): String = buildString(JSON.defaultOutputBuilderSize) {
        appendJSONLinesTo(this)
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun outputTo(out: IntConsumer) {
        out.accept('['.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                array[i].outputTo(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept(']'.code)
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    @Deprecated("renamed to outputTo", ReplaceWith("outputTo(out)"))
    override fun output(out: IntConsumer) {
        out.accept('['.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                array[i].outputTo(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept(']'.code)
    }

    /**
     * Output in JSON Lines form to an [IntConsumer].
     */
    fun outputJSONLinesTo(out: IntConsumer) {
        var i = 0
        while (i < size) {
            array[i++].outputTo(out)
            out.accept('\n'.code)
        }
    }

    /**
     * Output in JSON Lines form to an [IntConsumer].
     */
    @Deprecated("renamed to outputJSONLinesTo", ReplaceWith("outputJSONLinesTo(out)"))
    fun outputJSONLines(out: IntConsumer) {
        var i = 0
        while (i < size) {
            array[i++].outputTo(out)
            out.accept('\n'.code)
        }
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    override suspend fun coOutputTo(out: CoOutput) {
        out.output('[')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                array[i].coOutputTo(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output(']')
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    @Deprecated("renamed to coOutputTo(out)", ReplaceWith("coOutputTo(out)"))
    override suspend fun coOutput(out: CoOutput) {
        out.output('[')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                array[i].coOutputTo(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output(']')
    }

    /**
     * Output in JSON lines form to a [CoOutput].
     */
    suspend fun coOutputJSONLinesTo(out: CoOutput) {
        var i = 0
        while (i < size) {
            array[i++].coOutputTo(out)
            out.output('\n')
            if (out is CoOutputFlushable)
                out.flush()
        }
    }

    /**
     * Output in JSON lines form to a [CoOutput].
     */
    @Deprecated("renamed to coOutputJSONLinesTo", ReplaceWith("coOutputJSONLinesTo(out)"))
    suspend fun coOutputJSONLines(out: CoOutput) {
        var i = 0
        while (i < size) {
            array[i++].coOutputTo(out)
            out.output('\n')
            if (out is CoOutputFlushable)
                out.flush()
        }
    }

    /**
     * Return `true` if the array is empty.
     */
    override fun isEmpty(): Boolean = size == 0

    /**
     * Get the value at the given index.
     */
    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override fun get(index: Int): JSONValue? = ImmutableCollection.get(array, size, index)

    /**
     * Get a sub-list of the array.
     */
    override fun subList(fromIndex: Int, toIndex: Int): JSONArray = when (fromIndex) {
        toIndex -> EMPTY
        0 -> JSONArray(array, toIndex)
        else -> JSONArray(array.copyOfRange(fromIndex, toIndex), toIndex - fromIndex)
    }

    /**
     * Apply a function to each item in the array.
     */
    fun forEachItem(func: (JSONValue?) -> Unit) {
        for (i in indices)
            func(array[i])
    }

    /**
     * Apply a function to each item in the array, supplying the index.
     */
    fun forEachItemIndexed(func: (Int, JSONValue?) -> Unit) {
        for (i in indices)
            func(i, array[i])
    }

    /**
     * Compare the array to another value, applying the rule in Java for comparing `List`s.
     */
    override fun equals(other: Any?): Boolean = this === other ||
            other is List<*> && size == other.size && indices.all { array[it] == other[it] }

    /**
     * Get the hash code for the array, applying the rule in Java for `List` hash codes.
     */
    override fun hashCode(): Int = fold(1) { a, b -> 31 * a + b.hashCode() }

    /**
     * Convert to a [String] (converts to JSON).
     */
    override fun toString(): String = toJSON()

    /** The value as a [JSONArray] (unnecessary when type is known statically). */
    @Deprecated("Unnecessary (value is known to be JSONArray)", ReplaceWith("this"))
    val asArray: JSONArray get() = this

    /** The value as a [JSONArray] or `null` (unnecessary when type is known statically).  */
    @Deprecated("Unnecessary (value is known to be JSONArray)", ReplaceWith("this"))
    val asArrayOrNull: JSONArray get() = this

    companion object {

        private val EMPTY_ARRAY = emptyArray<JSONValue?>()

        /** An empty [JSONArray]. */
        val EMPTY = JSONArray(EMPTY_ARRAY, 0)

        /**
         * Create a [JSONArray] from a `vararg` list of [JSONValue] items.
         */
        fun of(vararg items: JSONValue?): JSONArray =
                if (items.isEmpty()) EMPTY else JSONArray(items.copyOf(), items.size)

        /**
         * Create a [JSONArray] from a [List] of [JSONValue] items.
         */
        fun from(list: List<JSONValue?>): JSONArray =
                if (list.isEmpty()) EMPTY else JSONArray(list.toTypedArray(), list.size)

        /**
         * Create a [JSONArray] by applying the supplied [block] to a [Builder], and then taking the result.
         */
        fun build(block: Builder.() -> Unit): JSONArray = Builder(block = block).build()

    }

    /**
     * [JSONArray] builder class.
     */
    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) : AbstractBuilder<JSONValue>(arrayOfNulls(size)) {

        init {
            block()
        }

        /**
         * Add a [JSONValue].
         */
        fun add(value: JSONValue?) {
            internalAdd(value)
        }

        /**
         * Add a [JSONString] with the supplied value.
         */
        fun add(value: String) {
            add(JSONString(value))
        }

        /**
         * Add a [JSONInt] with the supplied value.
         */
        fun add(value: Int) {
            add(JSONInt.of(value))
        }

        /**
         * Add a [JSONLong] with the supplied value.
         */
        fun add(value: Long) {
            add(JSONLong.of(value))
        }

        /**
         * Add a [JSONDecimal] with the supplied value.
         */
        fun add(value: BigDecimal) {
            add(JSONDecimal.of(value))
        }

        /**
         * Add a [JSONBoolean] with the supplied value.
         */
        fun add(value: Boolean) {
            add(JSONBoolean.of(value))
        }

        /**
         * Build a [JSONArray] with the values added.
         */
        override fun build(): JSONArray = checkArray().let {
            (if (size == 0) EMPTY else JSONArray(it, size)).also { invalidate() }
        }

    }

}

/**
 * Create a [JSONArray] from a `vararg` list of [JSONValue]s.
 */
fun JSONArray(vararg items: JSONValue?): JSONArray = JSONArray.of(*items)
