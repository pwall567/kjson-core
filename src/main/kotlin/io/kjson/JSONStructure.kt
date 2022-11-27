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
import io.kjson.JSON.notType

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
        if (it is JSONString) it.value else it.notType(JSON.TargetType.STRING, key)
    }

    fun getLong(key: K): Long = get(key).let {
        if (it is JSONNumber && it.isLong()) it.toLong() else it.notType(JSON.TargetType.LONG, key)
    }

    fun getInt(key: K): Int = get(key).let {
        if (it is JSONNumber && it.isInt()) it.toInt() else it.notType(JSON.TargetType.INT, key)
    }

    fun getShort(key: K): Short = get(key).let {
        if (it is JSONNumber && it.isShort()) it.toShort() else it.notType(JSON.TargetType.SHORT, key)
    }

    fun getByte(key: K): Byte = get(key).let {
        if (it is JSONNumber && it.isByte()) it.toByte() else it.notType(JSON.TargetType.BYTE, key)
    }

    fun getULong(key: K): ULong = get(key).let {
        if (it is JSONNumber && it.isULong()) it.toULong() else it.notType(JSON.TargetType.ULONG, key)
    }

    fun getUInt(key: K): UInt = get(key).let {
        if (it is JSONNumber && it.isUInt()) it.toUInt() else it.notType(JSON.TargetType.UINT, key)
    }

    fun getUShort(key: K): UShort = get(key).let {
        if (it is JSONNumber && it.isUShort()) it.toUShort() else it.notType(JSON.TargetType.USHORT, key)
    }

    fun getUByte(key: K): UByte = get(key).let {
        if (it is JSONNumber && it.isUByte()) it.toUByte() else it.notType(JSON.TargetType.UBYTE, key)
    }

    fun getDecimal(key: K): BigDecimal = get(key).let {
        if (it is JSONNumber) it.toDecimal() else it.notType(JSON.TargetType.DECIMAL, key)
    }

    fun getBoolean(key: K): Boolean = get(key).let {
        if (it is JSONBoolean) it.value else it.notType(JSON.TargetType.BOOLEAN, key)
    }

    fun getArray(key: K): JSONArray = get(key).let {
        it.asArrayOrNull ?: it.notType(JSON.TargetType.ARRAY, key)
    }

    fun getObject(key: K): JSONObject = get(key).let {
        it.asObjectOrNull ?: it.notType(JSON.TargetType.OBJECT, key)
    }

}
