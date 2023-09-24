/*
 * @(#) JSONStructure.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2022, 2023 Peter Wall
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

import io.kjson.JSON.asArrayOrError
import io.kjson.JSON.asBooleanOrError
import io.kjson.JSON.asByteOrError
import io.kjson.JSON.asDecimalOrError
import io.kjson.JSON.asIntOrError
import io.kjson.JSON.asLongOrError
import io.kjson.JSON.asObjectOrError
import io.kjson.JSON.asShortOrError
import io.kjson.JSON.asStringOrError
import io.kjson.JSON.asUByteOrError
import io.kjson.JSON.asUIntOrError
import io.kjson.JSON.asULongOrError
import io.kjson.JSON.asUShortOrError

/**
 * A sealed interface to specify the [JSONValue] classes that represent structured types (array and object).
 *
 * @author  Peter Wall
 * @param   K       the key type (`Int` for [JSONArray] or `String` for [JSONObject])
 */
sealed interface JSONStructure<K: Any> : JSONValue {

    /** The size of the structure. */
    val size: Int

    /**
     * Get the nominated value.
     */
    operator fun get(key: K): JSONValue?

    /**
     * Return `true` if the structure is empty.
     */
    fun isEmpty(): Boolean

    /**
     * Return `true` if the structure is not empty.
     */
    fun isNotEmpty(): Boolean = !isEmpty()

    /**
     * Get the nominated value os a [String], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getString(key: K): String = get(key).asStringOrError(key = key)

    /**
     * Get the nominated value os a [Long], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getLong(key: K): Long = get(key).asLongOrError(key = key)

    /**
     * Get the nominated value os a [Int], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getInt(key: K): Int = get(key).asIntOrError(key = key)

    /**
     * Get the nominated value os a [Short], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getShort(key: K): Short = get(key).asShortOrError(key = key)

    /**
     * Get the nominated value os a [Byte], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getByte(key: K): Byte = get(key).asByteOrError(key = key)

    /**
     * Get the nominated value os a [ULong], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getULong(key: K): ULong = get(key).asULongOrError(key = key)

    /**
     * Get the nominated value os a [UInt], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getUInt(key: K): UInt = get(key).asUIntOrError(key = key)

    /**
     * Get the nominated value os a [UShort], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getUShort(key: K): UShort = get(key).asUShortOrError(key = key)

    /**
     * Get the nominated value os a [UByte], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getUByte(key: K): UByte = get(key).asUByteOrError(key = key)

    /**
     * Get the nominated value os a [BigDecimal], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getDecimal(key: K): BigDecimal = get(key).asDecimalOrError(key = key)

    /**
     * Get the nominated value os a [Boolean], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getBoolean(key: K): Boolean = get(key).asBooleanOrError(key = key)

    /**
     * Get the nominated value os a [JSONArray], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getArray(key: K): JSONArray = get(key).asArrayOrError(key = key)

    /**
     * Get the nominated value os a [JSONObject], or throw a [JSONIncorrectTypeException] if it is of the wrong type.
     */
    fun getObject(key: K): JSONObject = get(key).asObjectOrError(key = key)

}
