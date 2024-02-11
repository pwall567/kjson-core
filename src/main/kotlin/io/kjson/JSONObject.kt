/*
 * @(#) JSONObject.kt
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

import io.kjson.JSON.appendTo
import io.kjson.JSON.coOutput
import io.kjson.JSON.output
import io.kjson.util.AbstractBuilder
import net.pwall.json.JSONCoFunctions.outputString
import net.pwall.json.JSONFunctions
import net.pwall.util.CoOutput
import net.pwall.util.ImmutableCollection
import net.pwall.util.ImmutableMap
import net.pwall.util.output

/**
 * A JSON object.
 *
 * @author  Peter Wall
 */
class JSONObject internal constructor(private val array: Array<JSONProperty>, override val size: Int) :
        JSONStructure<String>, Map<String, JSONValue?>, List<JSONProperty> {

    private val immutableMap = ImmutableMap<String, JSONValue?>(array, size)

    /** A [Set] of the [Map.Entry] objects in the object. */
    override val entries: Set<Map.Entry<String, JSONValue?>>
        get() = immutableMap.entries

    /** A [Set] of the keys (property names) in the object. */
    override val keys: Set<String>
        get() = immutableMap.keys

    /** A [Collection] of the property values in the object. */
    override val values: Collection<JSONValue?>
        get() = immutableMap.values

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) {
        a.append('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                JSONFunctions.appendString(a, property.key, false)
                a.append(':')
                property.value.appendTo(a)
                if (++i >= size)
                    break
                a.append(',')
            }
        }
        a.append('}')
    }

    /**
     * Convert to a JSON string.
     */
    override fun toJSON(): String = if (isEmpty()) "{}" else buildString { appendTo(this) }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun output(out: IntConsumer) {
        out.accept('{'.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                JSONFunctions.outputString(property.key, false, out)
                out.accept(':'.code)
                property.value.output(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept('}'.code)
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    override suspend fun coOutput(out: CoOutput) {
        out.output('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                out.outputString(property.key, false)
                out.output(':')
                property.value.coOutput(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output('}')
    }

    /**
     * Return `true` if the object is empty.
     */
    @Suppress("ReplaceSizeZeroCheckWithIsEmpty")
    override fun isEmpty(): Boolean = size == 0

    /**
     * Return `true` if the object contains the specified key (property name).
     */
    override fun containsKey(key: String): Boolean = immutableMap.containsKey(key)

    /**
     * Return `true` if the object contains the property value.
     */
    override fun containsValue(value: JSONValue?): Boolean = immutableMap.containsValue(value)

    /**
     * Get the value for the property with the specified name, or `null` if it is not present.
     */
    override fun get(key: String): JSONValue? = immutableMap[key]

    /**
     * Perform a function with each property in turn (the function takes two parameters, the name [String] and value
     * [JSONValue]`?`).
     */
    fun forEachEntry(func: (String, JSONValue?) -> Unit) {
        repeat(size) {
            val entry = array[it]
            func(entry.key, entry.value)
        }
    }

    /**
     * Perform a function with each property name in turn (the function takes a single parameters, the name [String]).
     */
    fun forEachKey(func: (String) -> Unit) {
        repeat(size) { func(array[it].key) }
    }

    /**
     * Perform a function with each property value in turn (the function takes a single parameters, the value
     * [JSONValue]`?`).
     */
    fun forEachValue(func: (JSONValue?) -> Unit) {
        repeat(size) { func(array[it].value) }
    }

    /**
     * Compare the object to another value, applying the rule in Java for comparing `Map`s.
     */
    @Suppress("SuspiciousEqualsCombination")
    override fun equals(other: Any?): Boolean = this === other || other is Map<*, *> && immutableMap == other

    /**
     * Get the hash code for the object, applying the rule in Java for `Map` hash codes.
     */
    override fun hashCode(): Int = immutableMap.hashCode()

    /**
     * Convert to a [String] (converts to JSON).
     */
    override fun toString(): String = toJSON()

    /** The value as a [JSONObject] (unnecessary when type is known statically). */
    @Deprecated("Unnecessary (value is known to be JSONObject)", ReplaceWith("this"))
    val asObject: JSONObject
        get() = this

    /** The value as a [JSONObject] or `null` (unnecessary when type is known statically). */
    @Deprecated("Unnecessary (value is known to be JSONObject)", ReplaceWith("this"))
    val asObjectOrNull: JSONObject
        get() = this

    // List functions

    /**
     * Get an iterator over the object [JSONProperty]s.
     */
    @Suppress("unchecked_cast")
    override fun iterator(): Iterator<JSONProperty> = immutableMap.iterator() as Iterator<JSONProperty>

    /**
     * Get a [kotlin.collections.ListIterator] over the object [JSONProperty]s.
     */
    override fun listIterator(): kotlin.collections.ListIterator<JSONProperty> = ListIterator(array, size)

    /**
     * Get a [kotlin.collections.ListIterator] over the object [JSONProperty]s, specifying the start index.
     */
    override fun listIterator(index: Int): kotlin.collections.ListIterator<JSONProperty> =
            ListIterator(array, size, index)

    /**
     * Get a `JSONObject` containing the set of properties bounded by `fromIndex` (inclusive) and `toIndex` (exclusive).
     */
    override fun subList(fromIndex: Int, toIndex: Int): JSONObject {
        if (fromIndex == 0 && toIndex == size)
            return this
        if (fromIndex < 0 || toIndex > size || toIndex < fromIndex)
            throw IndexOutOfBoundsException()
        val newSize = toIndex - fromIndex
        if (newSize == 0)
            return EMPTY
        return JSONObject(array.copyOfRange(fromIndex, toIndex), newSize)
    }

    /**
     * Get the index of the last occurrence of the specified [JSONProperty], or -1 if the entry is not found.
     */
    override fun lastIndexOf(element: JSONProperty): Int {
        var index = size
        while (--index > 0)
            if (array[index] == element)
                return index
        return -1
    }

    /**
     * Return `true` if the object contains the nominated entry.
     */
    override fun contains(element: JSONProperty): Boolean = ImmutableCollection.contains(array, size, element)

    /**
     * Return `true` if the object contains all of the entries in another collection.
     */
    override fun containsAll(elements: Collection<JSONProperty>): Boolean = elements.all { contains(it) }

    /**
     * Get the [JSONProperty] at the nominated index.
     */
    override fun get(index: Int): JSONProperty {
        if (index >= size)
            throw IndexOutOfBoundsException(index.toString())
        return array[index]
    }

    /**
     * Get the index of the first occurrence of the specified [JSONProperty], or -1 if the entry is not found.
     */
    override fun indexOf(element: JSONProperty): Int {
        var index = 0
        while (index < size)
            if (array[index++] == element)
                return index
        return -1
    }

    companion object {

        /** An empty [JSONObject]. */
        val EMPTY = JSONObject(emptyArray(), 0)

        /**
         * Create a [JSONObject] from a `vararg` list of map entries.
         */
        fun of(vararg items: Pair<String, JSONValue?>): JSONObject =
            if (items.isEmpty()) EMPTY else Array(items.size) { i ->
                items[i].let { JSONProperty(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        /**
         * Create a [JSONObject] from a [Map].
         */
        fun from(map: Map<String, JSONValue?>): JSONObject = if (map.isEmpty()) EMPTY else
            map.entries.map { JSONProperty(it.key, it.value) }.toTypedArray().let {
                JSONObject(it, it.size)
            }

        /**
         * Create a [JSONObject] from a [List] of [Pair]s of name and value.
         */
        fun from(list: List<Pair<String, JSONValue?>>): JSONObject =
            if (list.isEmpty()) EMPTY else Array(list.size) { i ->
                list[i].let { JSONProperty(it.first, it.second) }
            }.let { JSONObject(it, it.size) }

        /**
         * Create a [JSONObject] from a [List] of [JSONProperty].
         */
        fun fromProperties(list: List<JSONProperty>): JSONObject =
            if (list.isEmpty()) EMPTY else JSONObject(list.toTypedArray(), list.size)

        /**
         * Create a [JSONObject] by applying the supplied [block] to a [Builder], and then taking the result.
         */
        fun build(block: Builder.() -> Unit): JSONObject = Builder(block = block).build()

    }

    /**
     * [JSONObject] builder class.
     */
    class Builder(size: Int = 8, block: Builder.() -> Unit = {}) : AbstractBuilder<JSONProperty>(Array(size) { null }) {

        init {
            block()
        }

        /**
         * Test whether an entry with the nominated key (name) already exists in the builder.
         */
        fun containsKey(name: String): Boolean = ImmutableMap.containsKey(checkArray(), size, name)

        /**
         * Add a [JSONValue] with the specified name.
         */
        fun add(property: JSONProperty) {
            // TODO consider configuration to allow duplicates
            if (containsKey(property.key))
                throw JSONException("Duplicate key - ${property.key}")
            internalAdd(property)
        }

        /**
         * Add a [JSONValue] with the specified name.
         */
        fun add(name: String, value: JSONValue?) {
            // TODO consider configuration to allow duplicates
            if (containsKey(name))
                throw JSONException("Duplicate key - $name")
            internalAdd(JSONProperty(name, value))
        }

        /**
         * Add a [JSONString] with the supplied value and name.
         */
        fun add(name: String, value: String) {
            add(name, JSONString(value))
        }

        /**
         * Add a [JSONInt] with the supplied value and name.
         */
        fun add(name: String, value: Int) {
            add(name, JSONInt.of(value))
        }

        /**
         * Add a [JSONLong] with the supplied value and name.
         */
        fun add(name: String, value: Long) {
            add(name, JSONLong.of(value))
        }

        /**
         * Add a [JSONDecimal] with the supplied value and name.
         */
        fun add(name: String, value: BigDecimal) {
            add(name, JSONDecimal.of(value))
        }

        /**
         * Add a [JSONBoolean] with the supplied value and name.
         */
        fun add(name: String, value: Boolean) {
            add(name, JSONBoolean.of(value))
        }

        /**
         * Remove and entry with the specified name.
         */
        fun remove(name: String) {
            checkArray().let {
                val index = ImmutableMap.findKey(it, size, name)
                if (index < 0)
                    throw JSONException("Key not found - $name")
                internalRemove(index)
            }
        }

        /**
         * Get an existing entry with the specified name.
         */
        fun get(name: String): JSONValue? = checkArray().let {
            val index = ImmutableMap.findKey(it, size, name)
            if (index >= 0) it[index]?.value else null
        }

        /**
         * Build a [JSONObject] with the entries added.
         */
        @Suppress("unchecked_cast")
        override fun build(): JSONObject = checkArray().let {
            (if (size == 0) EMPTY else JSONObject(it as Array<JSONProperty>, size)).also { invalidate() }
        }

    }

    /**
     * [kotlin.collections.ListIterator] implementation for [JSONObject].
     */
    internal class ListIterator(
        private val array: Array<JSONProperty>,
        private val size: Int,
        initialIndex: Int = 0,
    ) : kotlin.collections.ListIterator<JSONProperty> {

        private var index = initialIndex

        /**
         * Test whether iterator has any more elements.
         */
        override fun hasNext(): Boolean = index < size

        /**
         * Get the next element referenced by this iterator.
         */
        override fun next(): JSONProperty = if (hasNext())
            array[index++]
        else
            throw NoSuchElementException(index.toString())

        /**
         * Test whether the iterator has any preceding elements.
         */
        override fun hasPrevious(): Boolean = index > 0

        /**
         * Get the preceding element referenced by this iterator.
         */
        override fun previous(): JSONProperty = if (hasPrevious())
            array[--index]
        else
            throw NoSuchElementException(previousIndex().toString())

        /**
         * Get the index of the "next" element.
         */
        override fun nextIndex(): Int = index

        /**
         * Get the preceding element referenced by this iterator.
         */
        override fun previousIndex(): Int = index - 1

    }

}
