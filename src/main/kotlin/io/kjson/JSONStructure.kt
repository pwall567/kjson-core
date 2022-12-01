/*
 * @(#) JSONStructure.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2022 Peter Wall
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

import io.kjson.JSON.asArrayOrNull
import io.kjson.JSON.asObjectOrNull
import io.kjson.JSON.typeError

/**
 * A sealed interface to specify the [JSONValue] classes that represent structured types (array and object).
 *
 * @author  Peter Wall
 * @param   K       the key type (`Int` for [JSONArray] or `String` for [JSONObject])
 */
sealed interface JSONStructure<K: Any> : JSONValue {

    val size: Int

    operator fun get(key: K): JSONValue?

    fun isEmpty(): Boolean

    fun isNotEmpty(): Boolean = !isEmpty()

    fun getString(key: K): String = get(key).let {
        if (it is JSONString) it.value else it.typeError(String::class, key)
    }

    fun getLong(key: K): Long = get(key).let {
        if (it is JSONNumber && it.isLong()) it.toLong() else it.typeError(Long::class, key)
    }

    fun getInt(key: K): Int = get(key).let {
        if (it is JSONNumber && it.isInt()) it.toInt() else it.typeError(Int::class, key)
    }

    fun getShort(key: K): Short = get(key).let {
        if (it is JSONNumber && it.isShort()) it.toShort() else it.typeError(Short::class, key)
    }

    fun getByte(key: K): Byte = get(key).let {
        if (it is JSONNumber && it.isByte()) it.toByte() else it.typeError(Byte::class, key)
    }

    fun getULong(key: K): ULong = get(key).let {
        if (it is JSONNumber && it.isULong()) it.toULong() else it.typeError(ULong::class, key)
    }

    fun getUInt(key: K): UInt = get(key).let {
        if (it is JSONNumber && it.isUInt()) it.toUInt() else it.typeError(UInt::class, key)
    }

    fun getUShort(key: K): UShort = get(key).let {
        if (it is JSONNumber && it.isUShort()) it.toUShort() else it.typeError(UShort::class, key)
    }

    fun getUByte(key: K): UByte = get(key).let {
        if (it is JSONNumber && it.isUByte()) it.toUByte() else it.typeError(UByte::class, key)
    }

    fun getDecimal(key: K): BigDecimal = get(key).let {
        if (it is JSONNumber) it.toDecimal() else it.typeError(BigDecimal::class, key)
    }

    fun getBoolean(key: K): Boolean = get(key).let {
        if (it is JSONBoolean) it.value else it.typeError(Boolean::class, key)
    }

    fun getArray(key: K): JSONArray = get(key).let {
        it.asArrayOrNull ?: it.typeError(JSONArray::class, key)
    }

    fun getObject(key: K): JSONObject = get(key).let {
        it.asObjectOrNull ?: it.typeError(JSONObject::class, key)
    }

}
