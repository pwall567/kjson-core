/*
 * @(#) JSON.kt
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

import io.kjson.parser.Parser
import net.pwall.json.JSONFunctions.appendString
import net.pwall.json.JSONFunctions.displayString
import net.pwall.util.CoOutput
import net.pwall.util.output

/**
 * JSON core library - a set of functions to assist with the creation and output of JSON data.
 *
 * @author  Peter Wall
 */
object JSON {

    /**
     * Create a [JSONInt] from an [Int] value.
     */
    fun of(value: Int): JSONInt = JSONInt.of(value)

    /**
     * Create a [JSONLong] from a [Long] value.
     */
    fun of(value: Long): JSONLong = JSONLong.of(value)

    /**
     * Create a [JSONDecimal] from a [BigDecimal] value.
     */
    fun of(value: BigDecimal): JSONDecimal = JSONDecimal.of(value)

    /**
     * Create a [JSONString] from a [String] value.
     */
    fun of(value: String): JSONString = JSONString.of(value)

    /**
     * Create a [JSONBoolean] from a [Boolean] value.
     */
    fun of(value: Boolean): JSONBoolean = JSONBoolean.of(value)

    /**
     * Create a [JSONArray] from a `vararg` list of [JSONValue] items.
     */
    fun of(vararg items: JSONValue?): JSONArray = JSONArray.of(*items)

    /**
     * Create a [JSONArray] from a `vararg` list of [Pair] of name to value pairs.
     */
    fun of(vararg items: Pair<String, JSONValue?>): JSONObject = JSONObject.of(*items)

    /**
     * Parse a [String] to a [JSONValue] (or `null` if the string is `"null"`).
     */
    fun parse(json: String): JSONValue? = Parser.parse(json)

    /**
     * Parse a [String] to a [JSONValue] (guaranteed non-null).
     */
    fun parseNonNull(json: String): JSONValue = parse(json) ?: throw JSONException("JSON must not be \"null\"")

    /**
     * Parse a [String] to a [JSONArray].
     */
    fun parseArray(json: String): JSONArray = parse(json).asArray

    /**
     * Parse a [String] to a [JSONObject].
     */
    fun parseObject(json: String): JSONObject = parse(json).asObject

    /**
     * Parse a [String] to a [JSONArray], where the input is in JSON Lines form.
     */
    fun parseLines(jsonLines: String): JSONArray = Parser.parseLines(jsonLines)

    /**
     * Convert the receiver [JSONValue] to a JSON string (`"null"` if the receiver is `null`).
     */
    fun JSONValue?.toJSON(): String = this?.toJSON() ?: "null"

    /**
     * Append the receiver [JSONValue] as a JSON string (`"null"` if the receiver is `null`) to an [Appendable].
     */
    fun JSONValue?.appendTo(a: Appendable) {
        if (this == null)
            a.append("null")
        else
            appendTo(a)
    }

    /**
     * Output the receiver [JSONValue] as a JSON string (`"null"` if the receiver is `null`) to an [IntConsumer] (a
     * character at a time).
     */
    fun JSONValue?.output(out: IntConsumer) {
        if (this == null)
            out.accept("null")
        else
            output(out)
    }

    /**
     * Output the receiver [JSONValue] as a JSON string (`"null"` if the receiver is `null`) to a [CoOutput] (a
     * character at a time).
     */
    suspend fun JSONValue?.coOutput(out: CoOutput) {
        if (this == null)
            out.output("null")
        else
            coOutput(out)
    }

    /**
     * Append a [JSONValue] as a JSON string (`"null"` if the value is `null`) to the receiver [Appendable].
     */
    fun Appendable.appendJSONValue(json: JSONValue?): Appendable = apply {
        if (json == null)
            append("null")
        else
            json.appendTo(this)
    }

    /**
     * Convert the receiver [JSONValue] to a string suitable for use in human-readable messages.
     */
    fun JSONValue?.displayValue(maxString: Int = 21): String = when (this) {
        null -> "null"
        is JSONString -> displayString(value, maxString)
        is JSONArray -> if (isEmpty()) "[]" else "[ ... ]"
        is JSONObject -> if (isEmpty()) "{}" else "{ ... }"
        else -> toString()
    }

    /**
     * Convert the receiver [JSONValue] to a string of JSON with nominated values elided (replaced by a substitute
     * string).
     */
    fun JSONValue?.elidedValue(
        exclude: Collection<String>? = null,
        include: Collection<String>? = null,
        substitute: String = "****",
    ): String = when (this) {
        null -> "null"
        is JSONStructure<*> -> buildString { appendElided(this@elidedValue, exclude, include, substitute) }
        else -> toJSON()
    }

    private fun Appendable.appendElided(
        json: JSONValue?,
        exclude: Collection<String>?,
        include: Collection<String>?,
        substitute: String,
    ) {
        when (json) {
            null -> append("null")
            is JSONObject -> {
                append('{')
                if (json.isNotEmpty()) {
                    val iterator = json.entries.iterator()
                    while (true) {
                        val (key, value) = iterator.next()
                        appendString(this, key, false)
                        append(':')
                        if ((exclude == null || key !in exclude) && (include == null || key in include))
                            appendElided(value, exclude, include, substitute)
                        else
                            appendString(this, substitute, false)
                        if (!iterator.hasNext())
                            break
                        append(',')
                    }
                }
                append('}')
            }
            is JSONArray -> {
                append('[')
                if (json.isNotEmpty()) {
                    val iterator = json.iterator()
                    while (true) {
                        appendElided(iterator.next(), exclude, include, substitute)
                        if (!iterator.hasNext())
                            break
                        append(',')
                    }
                }
                append(']')
            }
            else -> json.appendTo(this)
        }
    }

    /** The value of the receiver [JSONValue] as a [String].  */
    val JSONValue?.asString: String
        get() = if (this is JSONString) value else typeError("String")

    /** The value of the receiver [JSONValue] as a [String] or `null`.  */
    val JSONValue?.asStringOrNull: String?
        get() = if (this is JSONString) value else null

    /** The value of the receiver [JSONValue] as a [Long].  */
    val JSONValue?.asLong: Long
        get() = if (this is JSONNumber && isLong()) toLong() else typeError("Long")

    /** The value of the receiver [JSONValue] as a [Long] or `null`.  */
    val JSONValue?.asLongOrNull: Long?
        get() = if (this is JSONNumber && isLong()) toLong() else null

    /** The value of the receiver [JSONValue] as an [Int].  */
    val JSONValue?.asInt: Int
        get() = if (this is JSONNumber && isInt()) toInt() else typeError("Int")

    /** The value of the receiver [JSONValue] as an [Int] or `null`.  */
    val JSONValue?.asIntOrNull: Int?
        get() = if (this is JSONNumber && isInt()) toInt() else null

    /** The value of the receiver [JSONValue] as a [Short].  */
    val JSONValue?.asShort: Short
        get() = if (this is JSONNumber && isShort()) toShort() else typeError("Short")

    /** The value of the receiver [JSONValue] as a [Short] or `null`.  */
    val JSONValue?.asShortOrNull: Short?
        get() = if (this is JSONNumber && isShort()) toShort() else null

    /** The value of the receiver [JSONValue] as a [Byte].  */
    val JSONValue?.asByte: Byte
        get() = if (this is JSONNumber && isByte()) toByte() else typeError("Byte")

    /** The value of the receiver [JSONValue] as a [Byte] or `null`.  */
    val JSONValue?.asByteOrNull: Byte?
        get() = if (this is JSONNumber && isByte()) toByte() else null

    /** The value of the receiver [JSONValue] as a [ULong].  */
    val JSONValue?.asULong: ULong
        get() = if (this is JSONNumber && isULong()) toULong() else typeError("ULong")

    /** The value of the receiver [JSONValue] as a [ULong] or `null`.  */
    val JSONValue?.asULongOrNull: ULong?
        get() = if (this is JSONNumber && isULong()) toULong() else null

    /** The value of the receiver [JSONValue] as a [UInt].  */
    val JSONValue?.asUInt: UInt
        get() = if (this is JSONNumber && isUInt()) toUInt() else typeError("UInt")

    /** The value of the receiver [JSONValue] as a [UInt] or `null`.  */
    val JSONValue?.asUIntOrNull: UInt?
        get() = if (this is JSONNumber && isUInt()) toUInt() else null

    /** The value of the receiver [JSONValue] as a [UShort].  */
    val JSONValue?.asUShort: UShort
        get() = if (this is JSONNumber && isUShort()) toUShort() else typeError("UShort")

    /** The value of the receiver [JSONValue] as a [UShort] or `null`.  */
    val JSONValue?.asUShortOrNull: UShort?
        get() = if (this is JSONNumber && isUShort()) toUShort() else null

    /** The value of the receiver [JSONValue] as a [UByte].  */
    val JSONValue?.asUByte: UByte
        get() = if (this is JSONNumber && isUByte()) toUByte() else typeError("UByte")

    /** The value of the receiver [JSONValue] as a [UByte] or `null`.  */
    val JSONValue?.asUByteOrNull: UByte?
        get() = if (this is JSONNumber && isUByte()) toUByte() else null

    /** The value of the receiver [JSONValue] as a [BigDecimal].  */
    val JSONValue?.asDecimal: BigDecimal
        get() = if (this is JSONNumber) toDecimal() else typeError("BigDecimal")

    /** The value of the receiver [JSONValue] as a [BigDecimal] or `null`.  */
    val JSONValue?.asDecimalOrNull: BigDecimal?
        get() = if (this is JSONNumber) toDecimal() else null

    /** The value of the receiver [JSONValue] as a [Boolean].  */
    val JSONValue?.asBoolean: Boolean
        get() = if (this is JSONBoolean) value else typeError("Boolean")

    /** The value of the receiver [JSONValue] as a [Boolean] or `null`.  */
    val JSONValue?.asBooleanOrNull: Boolean?
        get() = if (this is JSONBoolean) value else null

    /** The value of the receiver [JSONValue] as a [JSONArray].  */
    val JSONValue?.asArray: JSONArray
        get() = if (this is JSONArray) this else typeError("JSONArray")

    /** The value of the receiver [JSONValue] as a [JSONArray] or `null`.  */
    val JSONValue?.asArrayOrNull: JSONArray?
        get() = this as? JSONArray

    /** The value of the receiver [JSONValue] as a [JSONObject].  */
    val JSONValue?.asObject: JSONObject
        get() = if (this is JSONObject) this else typeError("JSONObject")

    /** The value of the receiver [JSONValue] as a [JSONObject] or `null`.  */
    val JSONValue?.asObjectOrNull: JSONObject?
        get() = this as? JSONObject

    /**
     * Get the receiver [JSONValue] as a [String], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asStringOrError(
        target: String = "String",
        key: Any? = null,
        nodeName: String = "Node",
    ): String = if (this is JSONString) value else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [Long], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asLongOrError(
        target: String = "Long",
        key: Any? = null,
        nodeName: String = "Node",
    ): Long = if (this is JSONNumber && isLong()) toLong() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as an [Int], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asIntOrError(
        target: String = "Int",
        key: Any? = null,
        nodeName: String = "Node",
    ): Int = if (this is JSONNumber && isInt()) toInt() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [Short], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asShortOrError(
        target: String = "Short",
        key: Any? = null,
        nodeName: String = "Node",
    ): Short = if (this is JSONNumber && isShort()) toShort() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [Byte], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asByteOrError(
        target: String = "Byte",
        key: Any? = null,
        nodeName: String = "Node",
    ): Byte = if (this is JSONNumber && isByte()) toByte() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [ULong], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asULongOrError(
        target: String = "ULong",
        key: Any? = null,
        nodeName: String = "Node",
    ): ULong = if (this is JSONNumber && isULong()) toULong() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [UInt], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asUIntOrError(
        target: String = "UInt",
        key: Any? = null,
        nodeName: String = "Node",
    ): UInt = if (this is JSONNumber && isUInt()) toUInt() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [UShort], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asUShortOrError(
        target: String = "UShort",
        key: Any? = null,
        nodeName: String = "Node",
    ): UShort = if (this is JSONNumber && isUShort()) toUShort() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [UByte], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asUByteOrError(
        target: String = "UByte",
        key: Any? = null,
        nodeName: String = "Node",
    ): UByte = if (this is JSONNumber && isUByte()) toUByte() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [BigDecimal], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asDecimalOrError(
        target: String = "BigDecimal",
        key: Any? = null,
        nodeName: String = "Node",
    ): BigDecimal = if (this is JSONNumber) toDecimal() else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [Boolean], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asBooleanOrError(
        target: String = "Boolean",
        key: Any? = null,
        nodeName: String = "Node",
    ): Boolean = if (this is JSONBoolean) value else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [JSONArray], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asArrayOrError(
        target: String = "JSONArray",
        key: Any? = null,
        nodeName: String = "Node",
    ): JSONArray = if (this is JSONArray) this else typeError(target, key, nodeName)

    /**
     * Get the receiver [JSONValue] as a [JSONObject], or throw a [JSONTypeException] if incorrect).
     */
    fun JSONValue?.asObjectOrError(
        target: String = "JSONObject",
        key: Any? = null,
        nodeName: String = "Node",
    ): JSONObject = if (this is JSONObject) this else typeError(target, key, nodeName)

    /**
     * Throw a [JSONTypeException] with the given parameters.
     */
    fun JSONValue?.typeError(
        target: String,
        key: Any? = null,
        nodeName: String = "Node",
    ): Nothing {
        throw JSONTypeException(
            nodeName = nodeName,
            target = target,
            value = this,
            key = key,
        )
    }

    /**
     * Output a [CharSequence] (_e.g._ a [String]) to an [IntConsumer] one character at a time.
     */
    fun IntConsumer.accept(cs: CharSequence) {
        for (i in cs.indices)
            accept(cs[i].code)
    }

}
