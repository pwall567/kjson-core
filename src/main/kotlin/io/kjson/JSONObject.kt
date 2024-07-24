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
import io.kjson.JSON.coOutputTo
import io.kjson.JSON.outputTo
import io.kjson.util.AbstractBuilder
import net.pwall.json.JSONCoFunctions.outputString
import net.pwall.json.JSONFunctions
import net.pwall.util.CoOutput
import net.pwall.util.ImmutableList
import net.pwall.util.ImmutableMap
import net.pwall.util.ImmutableMapEntry
import net.pwall.util.output

/**
 * A JSON object.
 *
 * @author  Peter Wall
 */
class JSONObject internal constructor(private val array: Array<out Property>, override val size: Int) :
        JSONStructure<String>, Map<String, JSONValue?> by ImmutableMap<String, JSONValue?>(array, size),
        List<JSONObject.Property> by ImmutableList(array, size) {

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
    override fun toJSON(): String = if (isEmpty())
        "{}"
    else
        buildString(JSON.defaultOutputBuilderSize) {
            appendTo(this)
        }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    override fun outputTo(out: IntConsumer) {
        out.accept('{'.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                JSONFunctions.outputString(property.key, false, out)
                out.accept(':'.code)
                property.value.outputTo(out)
                if (++i >= size)
                    break
                out.accept(','.code)
            }
        }
        out.accept('}'.code)
    }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    @Deprecated("renamed to outputTo", ReplaceWith("outputTo(out)"))
    override fun output(out: IntConsumer) {
        out.accept('{'.code)
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                JSONFunctions.outputString(property.key, false, out)
                out.accept(':'.code)
                property.value.outputTo(out)
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
    override suspend fun coOutputTo(out: CoOutput) {
        out.output('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                out.outputString(property.key, false)
                out.output(':')
                property.value.coOutputTo(out)
                if (++i >= size)
                    break
                out.output(',')
            }
        }
        out.output('}')
    }

    /**
     * Output as a JSON string to a [CoOutput].
     */
    @Deprecated("renamed to coOutputTo", ReplaceWith("coOutputTo(out)"))
    override suspend fun coOutput(out: CoOutput) {
        out.output('{')
        if (isNotEmpty()) {
            var i = 0
            while (true) {
                val property = array[i]
                out.outputString(property.key, false)
                out.output(':')
                property.value.coOutputTo(out)
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
     * Perform a function with each property in turn (the function takes two parameters, the name [String] and value
     * [JSONValue]`?`).
     */
    fun forEachEntry(func: (String, JSONValue?) -> Unit) {
        for (i in indices) {
            val entry = array[i]
            func(entry.key, entry.value)
        }
    }

    /**
     * Perform a function with each property in turn (the function takes a single parameter, the [Property]).
     */
    fun forEachProperty(func: (Property) -> Unit) {
        for (i in indices)
            func(array[i])
    }

    /**
     * Perform a function with each property name in turn (the function takes a single parameters, the name [String]).
     */
    fun forEachKey(func: (String) -> Unit) {
        for (i in indices)
            func(array[i].key)
    }

    /**
     * Perform a function with each property value in turn (the function takes a single parameters, the value
     * [JSONValue]`?`).
     */
    fun forEachValue(func: (JSONValue?) -> Unit) {
        for (i in indices)
            func(array[i].value)
    }

    /**
     * Compare the object to another value, applying the rule in Java for comparing `Map`s, or the rule for comparing
     * `List`s.
     */
    override fun equals(other: Any?): Boolean = if (this === other) true else
        when (other) {
            is Map<*, *> -> size == other.size && indices.all { other[array[it].name] == array[it].value }
            is List<*> -> size == other.size && indices.all { array[it] == other[it] }
            else -> false
        }

    /**
     * Get the hash code for the object, applying the rule in Java for `Map` hash codes.  Note that the Java rule for
     * `List` hash codes is different, so it will not be possible to use this class in a hashing context (_e.g._ in a
     * `HashSet`) with other `List` implementations.
     */
    override fun hashCode(): Int = indices.sumOf { array[it].hashCode() }

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

    /**
     * Get a `JSONObject` containing the set of [Property]s bounded by `fromIndex` (inclusive) and `toIndex`
     * (exclusive).
     */
    override fun subList(fromIndex: Int, toIndex: Int): JSONObject = when (fromIndex) {
        toIndex -> EMPTY
        0 -> JSONObject(array, toIndex)
        else -> JSONObject(array.copyOfRange(fromIndex, toIndex), toIndex - fromIndex)
    }

    /**
     * A class to represent a property (name-value pair).
     */
    class Property(key: String, value: JSONValue?) : ImmutableMapEntry<String, JSONValue?>(key, value) {

        constructor(pair: Pair<String, JSONValue?>) : this(pair.first, pair.second)

        val name: String
            get() = key

    }

    companion object {

        /** An empty [JSONObject]. */
        val EMPTY = JSONObject(emptyArray(), 0)

        /**
         * Create a [JSONObject] from a `vararg` list of [Pair]s of name and value.
         */
        fun of(vararg items: Pair<String, JSONValue?>): JSONObject =
                if (items.isEmpty()) EMPTY else JSONObject(Array(items.size) { i -> Property(items[i]) }, items.size)

        /**
         * Create a [JSONObject] from a [Map].
         */
        fun from(map: Map<String, JSONValue?>): JSONObject = if (map.isEmpty()) EMPTY else
                JSONObject(map.entries.map { Property(it.key, it.value) }.toTypedArray(), map.size)

        /**
         * Create a [JSONObject] from a [List] of [Pair]s of name and value.
         */
        fun from(list: List<Pair<String, JSONValue?>>): JSONObject =
                if (list.isEmpty()) EMPTY else JSONObject(Array(list.size) { i -> Property(list[i]) }, list.size)

        /**
         * Create a [JSONObject] from a [List] of [Property].
         */
        fun fromProperties(list: List<Property>): JSONObject =
                if (list.isEmpty()) EMPTY else JSONObject(list.toTypedArray(), list.size)

        /**
         * Create a [JSONObject] by applying the supplied block to a [Builder], and then taking the result.
         */
        fun build(
            size: Int = 8,
            duplicateKeyOption: DuplicateKeyOption = DuplicateKeyOption.ERROR,
            errorKey: String? = null,
            block: Builder.() -> Unit
        ): JSONObject = Builder(size, duplicateKeyOption, errorKey, block).build()

    }

    /**
     * Option for handling duplicate keys.
     */
    enum class DuplicateKeyOption { ERROR, TAKE_FIRST, TAKE_LAST, CHECK_IDENTICAL }

    /**
     * [JSONObject] builder class.
     */
    class Builder(
        size: Int = 8,
        private val duplicateKeyOption: DuplicateKeyOption = DuplicateKeyOption.ERROR,
        private val errorKey: String? = null,
        block: Builder.() -> Unit = {}
    ) : AbstractBuilder<Property>(arrayOfNulls(size)) {

        init {
            block()
        }

        /**
         * Test whether an entry with the nominated key (name) already exists in the builder.
         */
        fun containsKey(name: String): Boolean = ImmutableMap.containsKey(checkArray(), size, name)

        /**
         * Add a [Property].
         */
        fun add(property: Property) {
            val array = checkArray()
            val index = ImmutableMap.findKey(array, size, property.key)
            if (index >= 0) {
                when (duplicateKeyOption) {
                    DuplicateKeyOption.ERROR -> throwDuplicateKeyError(property)
                    DuplicateKeyOption.TAKE_FIRST -> {}
                    DuplicateKeyOption.TAKE_LAST -> {
                        internalRemove(index)
                        internalAdd(property)
                    }
                    DuplicateKeyOption.CHECK_IDENTICAL -> {
                        if (array[index]?.value != property.value)
                            throwDuplicateKeyError(property)
                    }
                }
            }
            else
                internalAdd(property)
        }

        private fun throwDuplicateKeyError(property: Property): Nothing {
            throw JSONException("Duplicate key - ${property.key}", errorKey)
        }

        /**
         * Add a [JSONValue] with the specified name.
         */
        fun add(name: String, value: JSONValue?) {
            add(Property(name, value))
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
            (if (size == 0) EMPTY else JSONObject(it as Array<Property>, size)).also { invalidate() }
        }

    }

}
