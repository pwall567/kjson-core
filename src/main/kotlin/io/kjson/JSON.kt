/*
 * @(#) JSON.kt
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

import io.kjson.parser.Parser
import net.pwall.json.JSONFunctions.displayString

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

    fun parseArray(json: String): JSONArray = parse(json).asArray

    fun parseObject(json: String): JSONObject = parse(json).asObject

    fun JSONValue?.toJSON(): String = this?.toJSON() ?: "null"

    fun JSONValue?.appendTo(a: Appendable) {
        if (this == null)
            a.append("null")
        else
            appendTo(a)
    }

    fun Appendable.appendJSONValue(json: JSONValue?): Appendable = apply {
        if (json == null)
            append("null")
        else
            json.appendTo(this)
    }

    fun JSONValue?.displayValue(): String = when (this) {
        null -> "null"
        is JSONString -> displayString(value, 21)
        is JSONArray -> when (size) {
            0 -> "[]"
            1 -> "[${this[0].displayValue()}]"
            else -> "[...]"
        }
        is JSONObject -> when (size) {
            0 -> "{}"
            1 -> entries.iterator().next().let { "{${displayString(it.key, 21)}:${it.value.displayValue()}}" }
            else -> "{...}"
        }
        else -> toString()
    }

    val JSONValue?.asString: String
        get() = asStringOrNull ?: throw JSONException("Not a string - ${displayValue()}")

    val JSONValue?.asStringOrNull: String?
        get() = (this as? JSONString)?.value

    val JSONValue?.asLong: Long
        get() = asLongOrNull ?: throw JSONException("Not a long - ${displayValue()}")

    val JSONValue?.asLongOrNull: Long?
        get() = (this as? JSONNumberValue)?.let { if (it.isLong()) it.toLong() else null }

    val JSONValue?.asInt: Int
        get() = asIntOrNull ?: throw JSONException("Not an int - ${displayValue()}")

    val JSONValue?.asIntOrNull: Int?
        get() = (this as? JSONNumberValue)?.let { if (it.isInt()) it.toInt() else null }

    val JSONValue?.asShort: Short
        get() = asShortOrNull ?: throw JSONException("Not a short - ${displayValue()}")

    val JSONValue?.asShortOrNull: Short?
        get() = (this as? JSONNumberValue)?.let { if (it.isShort()) it.toShort() else null }

    val JSONValue?.asByte: Byte
        get() = asByteOrNull ?: throw JSONException("Not a byte - ${displayValue()}")

    val JSONValue?.asByteOrNull: Byte?
        get() = (this as? JSONNumberValue)?.let { if (it.isByte()) it.toByte() else null }

    val JSONValue?.asULong: ULong
        get() = asULongOrNull ?: throw JSONException("Not an unsigned long - ${displayValue()}")

    val JSONValue?.asULongOrNull: ULong?
        get() = (this as? JSONNumberValue)?.let { if (it.isULong()) it.toULong() else null }

    val JSONValue?.asUInt: UInt
        get() = asUIntOrNull ?: throw JSONException("Not an unsigned int - ${displayValue()}")

    val JSONValue?.asUIntOrNull: UInt?
        get() = (this as? JSONNumberValue)?.let { if (it.isUInt()) it.toUInt() else null }

    val JSONValue?.asUShort: UShort
        get() = asUShortOrNull ?: throw JSONException("Not an unsigned short - ${displayValue()}")

    val JSONValue?.asUShortOrNull: UShort?
        get() = (this as? JSONNumberValue)?.let { if (it.isUShort()) it.toUShort() else null }

    val JSONValue?.asUByte: UByte
        get() = asUByteOrNull ?: throw JSONException("Not an unsigned byte - ${displayValue()}")

    val JSONValue?.asUByteOrNull: UByte?
        get() = (this as? JSONNumberValue)?.let { if (it.isUByte()) it.toUByte() else null }

    val JSONValue?.asDecimal: BigDecimal
        get() = asDecimalOrNull ?: throw JSONException("Not a decimal - ${displayValue()}")

    val JSONValue?.asDecimalOrNull: BigDecimal?
        get() = (this as? JSONNumberValue)?.toDecimal()

    val JSONValue?.asBoolean: Boolean
        get() = asBooleanOrNull ?: throw JSONException("Not a boolean - ${displayValue()}")

    val JSONValue?.asBooleanOrNull: Boolean?
        get() = (this as? JSONBoolean)?.value

    val JSONValue?.asArray: JSONArray
        get() = asArrayOrNull ?: throw JSONException("Not an array - ${displayValue()}")

    val JSONValue?.asArrayOrNull: JSONArray?
        get() = (this as? JSONArray)

    val JSONValue?.asObject: JSONObject
        get() = asObjectOrNull ?: throw JSONException("Not an object - ${displayValue()}")

    val JSONValue?.asObjectOrNull: JSONObject?
        get() = (this as? JSONObject)

}
