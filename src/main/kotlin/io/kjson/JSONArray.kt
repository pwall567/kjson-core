/*
 * @(#) JSONArray.kt
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
import java.util.Arrays
import java.util.function.IntConsumer

import io.kjson.JSON.appendTo
import io.kjson.JSON.coOutput
import io.kjson.JSON.output
import io.kjson.util.AbstractBuilder
import net.pwall.util.CoOutput
import net.pwall.util.CoOutputFlushable
import net.pwall.util.ImmutableList
import net.pwall.util.output

/**
 * A JSON array.  As allowed by the JSON specification, array members may be primitive types, objects, other arrays or
 * `null`.
 *
 * @author  Peter Wall
 */
class JSONArray internal constructor (array: Array<out JSONValue?>, override val size: Int) : JSONStructure<Int>,
        List<JSONValue?> {

    private val immutableList = ImmutableList<JSONValue?>(array, size)

    override fun appendTo(a: Appendable) {
        a.append('[')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                immutableList[i].appendTo(a)
                if (++i >= size)
                    break
                a.append(',')
            }
        }
        a.append(']')
    }

    fun appendJSONLines(a: Appendable) {
        var i = 0
        while (i < size) {
            immutableList[i++].appendTo(a)
            a.append('\n')
        }
    }

    override fun toJSON(): String = if (isEmpty()) "[]" else buildString { appendTo(this) }

    fun toJSONLines(): String = if (isEmpty()) "" else buildString { appendJSONLines(this) }

    override fun output(out: IntConsumer) {
        out.accept('['.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                immutableList[i].output(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept(']'.code)
    }

    fun outputJSONLines(out: IntConsumer) {
        var i = 0
        while (i < size) {
            immutableList[i++].output(out)
            out.accept('\n'.code)
        }
    }

    override suspend fun coOutput(out: CoOutput) {
        out.output('[')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                immutableList[i].coOutput(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output(']')
    }

    suspend fun coOutputJSONLines(out: CoOutput) {
        var i = 0
        while (i < size) {
            immutableList[i++].coOutput(out)
            out.output('\n')
            if (out is CoOutputFlushable)
                out.flush()
        }
    }

    override fun isEmpty(): Boolean = size == 0

    override fun contains(element: JSONValue?): Boolean = immutableList.contains(element)

    override fun iterator(): Iterator<JSONValue?> = immutableList.iterator()

    override fun containsAll(elements: Collection<JSONValue?>): Boolean = immutableList.containsAll(elements)

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override fun get(index: Int): JSONValue? = immutableList[index]

    override fun indexOf(element: JSONValue?): Int = immutableList.indexOf(element)

    override fun lastIndexOf(element: JSONValue?): Int = immutableList.lastIndexOf(element)

    override fun listIterator(): ListIterator<JSONValue?> = immutableList.listIterator()

    override fun listIterator(index: Int): ListIterator<JSONValue?> = immutableList.listIterator(index)

    override fun subList(fromIndex: Int, toIndex: Int): JSONArray {
        if (fromIndex == toIndex)
            return EMPTY
        val oldArray = immutableList.toArray(EMPTY_ARRAY)
        if (fromIndex == 0)
            return JSONArray(oldArray, toIndex)
        return JSONArray(oldArray.copyOfRange(fromIndex, toIndex), toIndex - fromIndex)
    }

    fun forEachItem(func: (JSONValue?) -> Unit) {
        repeat(size) { func(immutableList[it]) }
    }

    fun forEachItemIndexed(func: (Int, JSONValue?) -> Unit) {
        repeat(size) { func(it, immutableList[it]) }
    }

    @Suppress("SuspiciousEqualsCombination")
    override fun equals(other: Any?): Boolean = this === other || other is List<*> && immutableList == other

    override fun hashCode(): Int = immutableList.hashCode()

    override fun toString(): String = toJSON()

    @Deprecated("Unnecessary (value is known to be JSONArray)", ReplaceWith("this"))
    val asArray: JSONArray
        get() = this

    @Deprecated("Unnecessary (value is known to be JSONArray)", ReplaceWith("this"))
    val asArrayOrNull: JSONArray
        get() = this

    companion object {

        private val EMPTY_ARRAY = emptyArray<JSONValue?>()

        val EMPTY = JSONArray(EMPTY_ARRAY, 0)

        @Suppress("ReplaceJavaStaticMethodWithKotlinAnalog")
        fun of(vararg items: JSONValue?): JSONArray =
                if (items.isEmpty()) EMPTY else JSONArray(Arrays.copyOf(items, items.size), items.size)

        fun from(list: List<JSONValue?>): JSONArray =
                if (list.isEmpty()) EMPTY else JSONArray(list.toTypedArray(), list.size)

        fun build(block: Builder.() -> Unit): JSONArray = Builder(block = block).build()

    }

    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) : AbstractBuilder<JSONValue>(arrayOfNulls(size)) {

        init {
            block()
        }

        fun add(value: JSONValue?) {
            internalAdd(value)
        }

        fun add(value: String) {
            add(JSONString(value))
        }

        fun add(value: Int) {
            add(JSONInt.of(value))
        }

        fun add(value: Long) {
            add(JSONLong.of(value))
        }

        fun add(value: BigDecimal) {
            add(JSONDecimal.of(value))
        }

        fun add(value: Boolean) {
            add(JSONBoolean.of(value))
        }

        override fun build(): JSONArray = checkArray().let {
            (if (size == 0) EMPTY else JSONArray(it, size)).also { invalidate() }
        }

    }

}
