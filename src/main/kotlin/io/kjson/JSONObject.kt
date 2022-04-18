/*
 * @(#) JSONObject.kt
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

import io.kjson.JSON.appendTo
import net.pwall.json.JSONFunctions
import net.pwall.util.ImmutableMap
import net.pwall.util.ImmutableMap.createArray
import net.pwall.util.ImmutableMap.entry
import net.pwall.util.ImmutableMapEntry

/**
 * A JSON object.
 *
 * @author  Peter Wall
 */
class JSONObject internal constructor(array: Array<ImmutableMapEntry<String, JSONValue?>>, override val size: Int) :
        JSONStructure, Map<String, JSONValue?> {

    private val immutableMap = ImmutableMap<String, JSONValue?>(array, size)

    override fun appendTo(a: Appendable) {
        a.append('{')
        if (isNotEmpty()) {
            val entrySet = immutableMap.entries
            var i = 0
            while (true) {
                val entry = entrySet.get(i)
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

    override fun equals(other: Any?): Boolean = this === other || other is Map<*, *> && immutableMap == other

    override fun hashCode(): Int = immutableMap.hashCode()

    override fun toString(): String = toJSON()

    companion object {

        val EMPTY = JSONObject(emptyArray(), 0)

        fun of(vararg items: Pair<String, JSONValue?>): JSONObject =
            if (items.isEmpty()) EMPTY else Array<ImmutableMapEntry<String, JSONValue?>>(items.size) { i ->
                items[i].let { entry(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        fun from(map: Map<String, JSONValue?>): JSONObject =
            if (map.isEmpty()) EMPTY else map.entries.map { entry(it.key, it.value) }.toTypedArray().let {
                JSONObject(it, it.size)
            }

        fun from(list: List<Pair<String, JSONValue?>>): JSONObject =
            if (list.isEmpty()) EMPTY else Array<ImmutableMapEntry<String, JSONValue?>>(list.size) { i ->
                list[i].let { entry(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        fun build(block: Builder.() -> Unit): JSONObject = Builder(block = block).build()

    }

    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) {

        private var array: Array<ImmutableMapEntry<String, JSONValue?>>? = createArray(size)
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

        fun containsKey(name: String): Boolean {
            checkArray().let { validArray ->
                for (i in 0 until count)
                    if (validArray[i].key == name)
                        return true
            }
            return false
        }

        fun add(name: String, value: JSONValue?) {
            if (containsKey(name))
                throw JSONException("Duplicate key - $name")
            checkArray().let { validArray ->
                val len = validArray.size
                if (count >= len) {
                    val newArray: Array<ImmutableMapEntry<String, JSONValue?>> =
                            createArray(len + len.coerceAtMost(4096))
                    System.arraycopy(validArray, 0, newArray, 0, len)
                    newArray[count++] = entry(name, value)
                    array = newArray
                }
                else
                    validArray[count++] = entry(name, value)
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

        fun build(): JSONObject = checkArray().let {
            (if (count == 0) EMPTY else JSONObject(it, count)).also { array = null }
        }

    }

}
