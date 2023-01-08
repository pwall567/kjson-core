/*
 * @(#) JSONObject.kt
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

import io.kjson.JSON.appendTo
import io.kjson.JSON.coOutput
import io.kjson.JSON.output
import net.pwall.json.JSONCoFunctions.outputString
import net.pwall.json.JSONFunctions
import net.pwall.util.CoOutput
import net.pwall.util.ImmutableMap
import net.pwall.util.ImmutableMapEntry
import net.pwall.util.output

/**
 * A JSON object.
 *
 * @author  Peter Wall
 */
class JSONObject internal constructor(array: Array<ImmutableMapEntry<String, JSONValue?>>, override val size: Int) :
        JSONStructure<String>, Map<String, JSONValue?> {

    internal val immutableMap = ImmutableMap<String, JSONValue?>(array, size)

    override fun appendTo(a: Appendable) {
        a.append('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val entry = immutableMap.getEntry(i)
                JSONFunctions.appendString(a, entry.key, false)
                a.append(':')
                entry.value.appendTo(a)
                if (++i >= size)
                    break
                a.append(',')
            }
        }
        a.append('}')
    }

    override fun toJSON(): String = if (isEmpty()) "{}" else buildString { appendTo(this) }

    override fun output(out: IntConsumer) {
        out.accept('{'.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val entry = immutableMap.getEntry(i)
                JSONFunctions.outputString(entry.key, false, out)
                out.accept(':'.code)
                entry.value.output(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept('}'.code)
    }

    override suspend fun coOutput(out: CoOutput) {
        out.output('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val entry = immutableMap.getEntry(i)
                out.outputString(entry.key, false)
                out.output(':')
                entry.value.coOutput(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output('}')
    }

    @Suppress("ReplaceSizeZeroCheckWithIsEmpty")
    override fun isEmpty(): Boolean = size == 0

    override val entries: Set<Map.Entry<String, JSONValue?>>
        get() = immutableMap.entries

    override val keys: Set<String>
        get() = immutableMap.keys

    override val values: Collection<JSONValue?>
        get() = immutableMap.values

    override fun containsKey(key: String): Boolean = immutableMap.containsKey(key)

    override fun containsValue(value: JSONValue?): Boolean = immutableMap.containsValue(value)

    override fun get(key: String): JSONValue? = immutableMap[key]

    fun forEachEntry(func: (String, JSONValue?) -> Unit) {
        repeat(size) {
            val entry = immutableMap.getEntry(it)
            func(entry.key, entry.value)
        }
    }

    fun forEachKey(func: (String) -> Unit) {
        repeat(size) { func(immutableMap.getKey(it)) }
    }

    fun forEachValue(func: (JSONValue?) -> Unit) {
        repeat(size) { func(immutableMap.getValue(it)) }
    }

    @Suppress("SuspiciousEqualsCombination")
    override fun equals(other: Any?): Boolean = this === other || other is Map<*, *> && immutableMap == other

    override fun hashCode(): Int = immutableMap.hashCode()

    override fun toString(): String = toJSON()

    @Deprecated("Unnecessary (value is known to be JSONObject)", ReplaceWith("this"))
    val asObject: JSONObject
        get() = this

    @Deprecated("Unnecessary (value is known to be JSONObject)", ReplaceWith("this"))
    val asObjectOrNull: JSONObject
        get() = this

    companion object {

        val EMPTY = JSONObject(emptyArray(), 0)

        fun of(vararg items: Pair<String, JSONValue?>): JSONObject =
            if (items.isEmpty()) EMPTY else Array<ImmutableMapEntry<String, JSONValue?>>(items.size) { i ->
                items[i].let { ImmutableMap.entry(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        fun from(map: Map<String, JSONValue?>): JSONObject =
            if (map.isEmpty()) EMPTY else map.entries.map { ImmutableMap.entry(it.key, it.value) }.toTypedArray().let {
                JSONObject(it, it.size)
            }

        fun from(list: List<Pair<String, JSONValue?>>): JSONObject =
            if (list.isEmpty()) EMPTY else Array<ImmutableMapEntry<String, JSONValue?>>(list.size) { i ->
                list[i].let { ImmutableMap.entry(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        fun build(block: Builder.() -> Unit): JSONObject = Builder(block = block).build()

    }

    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) {

        private var array: Array<ImmutableMapEntry<String, JSONValue?>>? = ImmutableMap.createArray(size)
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

        fun containsKey(name: String): Boolean = ImmutableMap.containsKey(checkArray(), count, name)

        fun add(name: String, value: JSONValue?) {
            if (containsKey(name))
                throw JSONException("Duplicate key - $name")
            checkArray().let { validArray ->
                val len = validArray.size
                if (count >= len) {
                    val newArray: Array<ImmutableMapEntry<String, JSONValue?>> =
                            ImmutableMap.createArray(len + len.coerceAtMost(4096))
                    System.arraycopy(validArray, 0, newArray, 0, len)
                    newArray[count++] = ImmutableMap.entry(name, value)
                    array = newArray
                }
                else
                    validArray[count++] = ImmutableMap.entry(name, value)
            }
        }

        fun add(name: String, value: String) {
            add(name, JSONString(value))
        }

        fun add(name: String, value: Int) {
            add(name, JSONInt.of(value))
        }

        fun add(name: String, value: Long) {
            add(name, JSONLong.of(value))
        }

        fun add(name: String, value: BigDecimal) {
            add(name, JSONDecimal.of(value))
        }

        fun add(name: String, value: Boolean) {
            add(name, JSONBoolean.of(value))
        }

        fun remove(name: String) {
            checkArray().let {
                val index = ImmutableMap.findKey(it, count, name)
                if (index < 0)
                    throw JSONException("Key not found - $name")
                System.arraycopy(it, index + 1, it, index, count - index)
                count--
            }
        }

        fun get(name: String): JSONValue? = checkArray().let {
            val index = ImmutableMap.findKey(it, count, name)
            if (index >= 0) it[index].value else null
        }

        fun build(): JSONObject = checkArray().let {
            (if (count == 0) EMPTY else JSONObject(it, count)).also { array = null }
        }

    }

}
