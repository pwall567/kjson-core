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

    fun of(value: Int): JSONInt = JSONInt.of(value)

    fun of(value: Long): JSONLong = JSONLong.of(value)

    fun of(value: BigDecimal): JSONDecimal = JSONDecimal.of(value)

    fun of(value: String): JSONString = JSONString.of(value)

    fun of(value: Boolean): JSONBoolean = JSONBoolean.of(value)

    fun of(vararg items: JSONValue?): JSONArray = JSONArray.of(*items)

    fun of(vararg items: Pair<String, JSONValue?>): JSONObject = JSONObject.of(*items)

    fun parse(json: String): JSONValue? = Parser.parse(json)

    fun parseNonNull(json: String): JSONValue = parse(json) ?: throw JSONException("JSON must not be \"null\"")

    fun parseArray(json: String): JSONArray = parse(json).asArray

    fun parseObject(json: String): JSONObject = parse(json).asObject

    fun parseLines(jsonLines: String): JSONArray = Parser.parseLines(jsonLines)

    fun JSONValue?.toJSON(): String = this?.toJSON() ?: "null"

    fun JSONValue?.appendTo(a: Appendable) {
        if (this == null)
            a.append("null")
        else
            appendTo(a)
    }

    fun JSONValue?.output(out: IntConsumer) {
        if (this == null)
            out.accept("null")
        else
            output(out)
    }

    suspend fun JSONValue?.coOutput(out: CoOutput) {
        if (this == null)
            out.output("null")
        else
            coOutput(out)
    }

    fun Appendable.appendJSONValue(json: JSONValue?): Appendable = apply {
        if (json == null)
            append("null")
        else
            json.appendTo(this)
    }

    fun JSONValue?.displayValue(maxString: Int = 21): String = when (this) {
        null -> "null"
        is JSONString -> displayString(value, maxString)
        is JSONArray -> when (size) {
            0 -> "[]"
            1 -> "[${this[0].displayValue()}]"
            else -> "[ ... ]"
        }
        is JSONObject -> when (size) {
            0 -> "{}"
            1 -> entries.iterator().next().let { "{${displayString(it.key, maxString)}:${it.value.displayValue()}}" }
            else -> "{ ... }"
        }
        else -> toString()
    }

    fun JSONValue?.elidedValue(
        exclude: Collection<String>? = null,
        include: Collection<String>? = null,
        substitute: String = "****",
    ): String = when (this) {
        null -> "null"
        is JSONStructure<*> -> buildString { appendElided(this, this@elidedValue, exclude, include, substitute) }
        else -> toJSON()
    }

    private fun appendElided(
        a: Appendable,
        json: JSONValue?,
        exclude: Collection<String>?,
        include: Collection<String>?,
        substitute: String,
    ) {
        when (json) {
            is JSONObject -> {
                a.append('{')
                if (json.isNotEmpty()) {
                    val iterator = json.entries.iterator()
                    while (true) {
                        val (key, value) = iterator.next()
                        appendString(a, key, false)
                        a.append(':')
                        if ((exclude == null || key !in exclude) && (include == null || key in include))
                            appendElided(a, value, exclude, include, substitute)
                        else
                            appendString(a, substitute, false)
                        if (!iterator.hasNext())
                            break
                        a.append(',')
                    }
                }
                a.append('}')
            }
            is JSONArray -> {
                a.append('[')
                if (json.isNotEmpty()) {
                    val iterator = json.iterator()
                    while (true) {
                        appendElided(a, iterator.next(), exclude, include, substitute)
                        if (!iterator.hasNext())
                            break
                        a.append(',')
                    }
                }
                a.append(']')
            }
            else -> a.appendJSONValue(json)
        }
    }

    val JSONValue?.asString: String
        get() = if (this is JSONString) value else typeError("String")

    val JSONValue?.asStringOrNull: String?
        get() = if (this is JSONString) value else null

    val JSONValue?.asLong: Long
        get() = if (this is JSONNumber && isLong()) toLong() else typeError("Long")

    val JSONValue?.asLongOrNull: Long?
        get() = if (this is JSONNumber && isLong()) toLong() else null

    val JSONValue?.asInt: Int
        get() = if (this is JSONNumber && isInt()) toInt() else typeError("Int")

    val JSONValue?.asIntOrNull: Int?
        get() = if (this is JSONNumber && isInt()) toInt() else null

    val JSONValue?.asShort: Short
        get() = if (this is JSONNumber && isShort()) toShort() else typeError("Short")

    val JSONValue?.asShortOrNull: Short?
        get() = if (this is JSONNumber && isShort()) toShort() else null

    val JSONValue?.asByte: Byte
        get() = if (this is JSONNumber && isByte()) toByte() else typeError("Byte")

    val JSONValue?.asByteOrNull: Byte?
        get() = if (this is JSONNumber && isByte()) toByte() else null

    val JSONValue?.asULong: ULong
        get() = if (this is JSONNumber && isULong()) toULong() else typeError("ULong")

    val JSONValue?.asULongOrNull: ULong?
        get() = if (this is JSONNumber && isULong()) toULong() else null

    val JSONValue?.asUInt: UInt
        get() = if (this is JSONNumber && isUInt()) toUInt() else typeError("UInt")

    val JSONValue?.asUIntOrNull: UInt?
        get() = if (this is JSONNumber && isUInt()) toUInt() else null

    val JSONValue?.asUShort: UShort
        get() = if (this is JSONNumber && isUShort()) toUShort() else typeError("UShort")

    val JSONValue?.asUShortOrNull: UShort?
        get() = if (this is JSONNumber && isUShort()) toUShort() else null

    val JSONValue?.asUByte: UByte
        get() = if (this is JSONNumber && isUByte()) toUByte() else typeError("UByte")

    val JSONValue?.asUByteOrNull: UByte?
        get() = if (this is JSONNumber && isUByte()) toUByte() else null

    val JSONValue?.asDecimal: BigDecimal
        get() = if (this is JSONNumber) toDecimal() else typeError("BigDecimal")

    val JSONValue?.asDecimalOrNull: BigDecimal?
        get() = if (this is JSONNumber) toDecimal() else null

    val JSONValue?.asBoolean: Boolean
        get() = if (this is JSONBoolean) value else typeError("Boolean")

    val JSONValue?.asBooleanOrNull: Boolean?
        get() = if (this is JSONBoolean) value else null

    val JSONValue?.asArray: JSONArray
        get() = if (this is JSONArray) this else typeError("JSONArray")

    val JSONValue?.asArrayOrNull: JSONArray?
        get() = this as? JSONArray

    val JSONValue?.asObject: JSONObject
        get() = if (this is JSONObject) this else typeError("JSONObject")

    val JSONValue?.asObjectOrNull: JSONObject?
        get() = this as? JSONObject

    fun JSONValue?.asStringOrError(
        target: String = "String",
        key: Any? = null,
        nodeName: String = "Node",
    ): String = if (this is JSONString) value else typeError(target, key, nodeName)

    fun JSONValue?.asLongOrError(
        target: String = "Long",
        key: Any? = null,
        nodeName: String = "Node",
    ): Long = if (this is JSONNumber && isLong()) toLong() else typeError(target, key, nodeName)

    fun JSONValue?.asIntOrError(
        target: String = "Int",
        key: Any? = null,
        nodeName: String = "Node",
    ): Int = if (this is JSONNumber && isInt()) toInt() else typeError(target, key, nodeName)

    fun JSONValue?.asShortOrError(
        target: String = "Short",
        key: Any? = null,
        nodeName: String = "Node",
    ): Short = if (this is JSONNumber && isShort()) toShort() else typeError(target, key, nodeName)

    fun JSONValue?.asByteOrError(
        target: String = "Byte",
        key: Any? = null,
        nodeName: String = "Node",
    ): Byte = if (this is JSONNumber && isByte()) toByte() else typeError(target, key, nodeName)

    fun JSONValue?.asULongOrError(
        target: String = "ULong",
        key: Any? = null,
        nodeName: String = "Node",
    ): ULong = if (this is JSONNumber && isULong()) toULong() else typeError(target, key, nodeName)

    fun JSONValue?.asUIntOrError(
        target: String = "UInt",
        key: Any? = null,
        nodeName: String = "Node",
    ): UInt = if (this is JSONNumber && isUInt()) toUInt() else typeError(target, key, nodeName)

    fun JSONValue?.asUShortOrError(
        target: String = "UShort",
        key: Any? = null,
        nodeName: String = "Node",
    ): UShort = if (this is JSONNumber && isUShort()) toUShort() else typeError(target, key, nodeName)

    fun JSONValue?.asUByteOrError(
        target: String = "UByte",
        key: Any? = null,
        nodeName: String = "Node",
    ): UByte = if (this is JSONNumber && isUByte()) toUByte() else typeError(target, key, nodeName)

    fun JSONValue?.asDecimalOrError(
        target: String = "BigDecimal",
        key: Any? = null,
        nodeName: String = "Node",
    ): BigDecimal = if (this is JSONNumber) toDecimal() else typeError(target, key, nodeName)

    fun JSONValue?.asBooleanOrError(
        target: String = "Boolean",
        key: Any? = null,
        nodeName: String = "Node",
    ): Boolean = if (this is JSONBoolean) value else typeError(target, key, nodeName)

    fun JSONValue?.asArrayOrError(
        target: String = "JSONArray",
        key: Any? = null,
        nodeName: String = "Node",
    ): JSONArray = if (this is JSONArray) this else typeError(target, key, nodeName)

    fun JSONValue?.asObjectOrError(
        target: String = "JSONObject",
        key: Any? = null,
        nodeName: String = "Node",
    ): JSONObject = if (this is JSONObject) this else typeError(target, key, nodeName)

    fun JSONValue?.typeError(
        target: String,
        key: Any? = null,
        nodeName: String = "Node",
    ): Nothing {
        throw JSONIncorrectTypeException(
            nodeName = nodeName,
            target = target,
            value = this,
            key = key,
        )
    }

    fun IntConsumer.accept(cs: CharSequence) {
        for (i in cs.indices)
            accept(cs[i].code)
    }

}
