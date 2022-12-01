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

import kotlin.reflect.KClass

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
        is JSONObject -> buildString { appendElided(this, this@elidedValue, exclude, include, substitute) }
        else -> toJSON()
    }

    private fun appendElided(
        a: Appendable,
        json: JSONValue?,
        exclude: Collection<String>?,
        include: Collection<String>?,
        substitute: String,
    ) {
        if (json is JSONObject) {
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
        else
            a.appendJSONValue(json)
    }

    val JSONValue?.asString: String
        get() = asStringOrNull ?: typeError(String::class)

    val JSONValue?.asStringOrNull: String?
        get() = (this as? JSONString)?.value

    val JSONValue?.asLong: Long
        get() = asLongOrNull ?: typeError(Long::class)

    val JSONValue?.asLongOrNull: Long?
        get() = (this as? JSONNumber)?.takeIf { it.isLong() }?.toLong()

    val JSONValue?.asInt: Int
        get() = asIntOrNull ?: typeError(Int::class)

    val JSONValue?.asIntOrNull: Int?
        get() = (this as? JSONNumber)?.takeIf { it.isInt() }?.toInt()

    val JSONValue?.asShort: Short
        get() = asShortOrNull ?: typeError(Short::class)

    val JSONValue?.asShortOrNull: Short?
        get() = (this as? JSONNumber)?.takeIf { it.isShort() }?.toShort()

    val JSONValue?.asByte: Byte
        get() = asByteOrNull ?: typeError(Byte::class)

    val JSONValue?.asByteOrNull: Byte?
        get() = (this as? JSONNumber)?.takeIf { it.isByte() }?.toByte()

    val JSONValue?.asULong: ULong
        get() = asULongOrNull ?: typeError(ULong::class)

    val JSONValue?.asULongOrNull: ULong?
        get() = (this as? JSONNumber)?.takeIf { it.isULong() }?.toULong()

    val JSONValue?.asUInt: UInt
        get() = asUIntOrNull ?: typeError(UInt::class)

    val JSONValue?.asUIntOrNull: UInt?
        get() = (this as? JSONNumber)?.takeIf { it.isUInt() }?.toUInt()

    val JSONValue?.asUShort: UShort
        get() = asUShortOrNull ?: typeError(UShort::class)

    val JSONValue?.asUShortOrNull: UShort?
        get() = (this as? JSONNumber)?.takeIf { it.isUShort() }?.toUShort()

    val JSONValue?.asUByte: UByte
        get() = asUByteOrNull ?: typeError(UByte::class)

    val JSONValue?.asUByteOrNull: UByte?
        get() = (this as? JSONNumber)?.takeIf { it.isUByte() }?.toUByte()

    val JSONValue?.asDecimal: BigDecimal
        get() = asDecimalOrNull ?: typeError(BigDecimal::class)

    val JSONValue?.asDecimalOrNull: BigDecimal?
        get() = (this as? JSONNumber)?.toDecimal()

    val JSONValue?.asBoolean: Boolean
        get() = asBooleanOrNull ?: typeError(Boolean::class)

    val JSONValue?.asBooleanOrNull: Boolean?
        get() = (this as? JSONBoolean)?.value

    val JSONValue?.asArray: JSONArray
        get() = asArrayOrNull ?: typeError(JSONArray::class)

    val JSONValue?.asArrayOrNull: JSONArray?
        get() = (this as? JSONArray)

    val JSONValue?.asObject: JSONObject
        get() = asObjectOrNull ?: typeError(JSONObject::class)

    val JSONValue?.asObjectOrNull: JSONObject?
        get() = (this as? JSONObject)

    fun JSONValue?.typeError(
        target: KClass<*>,
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
