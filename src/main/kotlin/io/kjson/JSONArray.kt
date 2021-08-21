/*
 * @(#) JSONArray.kt
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

import io.kjson.JSON.appendTo
import net.pwall.util.ImmutableList

/**
 * A JSON array.  As allowed by the JSON specification, array members may be primitive types, objects, other arrays or
 * `null`.
 *
 * @author  Peter Wall
 */
class JSONArray internal constructor (array: Array<out JSONValue?>, override val size: Int) : JSONValue,
        List<JSONValue?> {

    private val immutableList = ImmutableList<JSONValue?>(array, size)

    override fun appendTo(a: Appendable) {
        a.append('[')
        if (isNotEmpty()) {
            val iterator = iterator()
            while (true) {
                iterator.next().appendTo(a)
                if (!iterator.hasNext())
                    break
                a.append(',')
            }
        }
        a.append(']')
    }

    override fun isEmpty(): Boolean = immutableList.isEmpty

    override fun contains(element: JSONValue?): Boolean = immutableList.contains(element)

    override fun iterator(): Iterator<JSONValue?> = immutableList.iterator()

    override fun containsAll(elements: Collection<JSONValue?>): Boolean = immutableList.containsAll(elements)

    override fun get(index: Int): JSONValue? = immutableList[index]

    override fun indexOf(element: JSONValue?): Int = immutableList.indexOf(element)

    override fun lastIndexOf(element: JSONValue?): Int = immutableList.lastIndexOf(element)

    override fun listIterator(): ListIterator<JSONValue?> = immutableList.listIterator()

    override fun listIterator(index: Int): ListIterator<JSONValue?> = immutableList.listIterator(index)

    override fun subList(fromIndex: Int, toIndex: Int): List<JSONValue?> = immutableList.subList(fromIndex, toIndex)

    override fun equals(other: Any?): Boolean = this === other || other is List<*> && immutableList == other

    override fun hashCode(): Int = immutableList.hashCode()

    override fun toString(): String = toJSON()

    companion object {

        val EMPTY = JSONArray(emptyArray(), 0)

        fun of(vararg items: JSONValue?): JSONArray = if (items.isEmpty()) EMPTY else JSONArray(items, items.size)

        fun from(list: List<JSONValue?>): JSONArray =
                if (list.isEmpty()) EMPTY else JSONArray(list.toTypedArray(), list.size)

        fun build(block: Builder.() -> Unit): JSONArray = Builder(block = block).build()

    }

    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) {

        private var array: Array<JSONValue?>? = Array(size) { null }
        private var count: Int = 0

        init {
            block()
        }

        val size: Int
            get() {
                checkArray()
                return count
            }

        private fun checkArray() = array ?: throw JSONException("Builder is closed")

        fun add(value: JSONValue?) {
            checkArray().let { validArray ->
                val len = validArray.size
                if (count >= len) {
                    val newArray = Array(len + len.coerceAtMost(4096)) { i->
                        if (i < len) validArray[i] else null
                    }
                    newArray[count++] = value
                    array = newArray
                }
                else
                    validArray[count++] = value
            }
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

        fun build(): JSONArray = checkArray().let { JSONArray(it, count).also { array = null } }

    }

}
