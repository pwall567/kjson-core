/*
 * @(#) JSONStructureTest.kt
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

import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

import io.kjson.JSON.asString

class JSONStructureTest {

    @Test fun `should get string from object`() {
        val outer = JSONObject.build {
            add("data", "something")
        }
        expect("something") { outer.getString("data") }
    }

    @Test fun `should fail on getString from object when not a string`() {
        val outer = JSONObject.build {
            add("data", true)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getString("data") }.let {
            expect(JSON.TargetType.STRING) { it.target }
            expect("data") { it.key }
            expect(JSONBoolean.TRUE) { it.value }
            expect("Not a string (data) - true") { it.message }
        }
    }

    @Test fun `should get string from array`() {
        val outer = JSONArray.build {
            add("nice")
        }
        expect("nice") { outer.getString(0) }
    }

    @Test fun `should fail on getString from array when not a string`() {
        val outer = JSONArray.build {
            add(42)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getString(0) }.let {
            expect(JSON.TargetType.STRING) { it.target }
            expect(0) { it.key }
            expect(JSONInt(42)) { it.value }
            expect("Not a string (0) - 42") { it.message }
        }
    }

    @Test fun `should get long from object`() {
        val outer = JSONObject.build {
            add("lots", 1234567812345678)
        }
        expect(1234567812345678) { outer.getLong("lots") }
    }

    @Test fun `should fail on getLong from object when not a long`() {
        val outer = JSONObject.build {
            add("lots", "millions")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getLong("lots") }.let {
            expect(JSON.TargetType.LONG) { it.target }
            expect("lots") { it.key }
            expect(JSONString("millions")) { it.value }
            expect("Not a long (lots) - \"millions\"") { it.message }
        }
    }

    @Test fun `should get long from array`() {
        val outer = JSONArray.build {
            add(-2244668822446688)
        }
        expect(-2244668822446688) { outer.getLong(0) }
    }

    @Test fun `should fail on getLong from array when not a long`() {
        val outer = JSONArray.build {
            add(BigDecimal("1.555"))
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getLong(0) }.let {
            expect(JSON.TargetType.LONG) { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.555")) { it.value }
            expect("Not a long (0) - 1.555") { it.message }
        }
    }

    @Test fun `should get int from object`() {
        val outer = JSONObject.build {
            add("number", 12345678)
        }
        expect(12345678) { outer.getInt("number") }
    }

    @Test fun `should fail on getInt from object when not an int`() {
        val outer = JSONObject.build {
            add("number", "trouble")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getInt("number") }.let {
            expect(JSON.TargetType.INT) { it.target }
            expect("number") { it.key }
            expect(JSONString("trouble")) { it.value }
            expect("Not an int (number) - \"trouble\"") { it.message }
        }
    }

    @Test fun `should get int from array`() {
        val outer = JSONArray.build {
            add(-22446688)
        }
        expect(-22446688) { outer.getInt(0) }
    }

    @Test fun `should fail on getInt from array when not an int`() {
        val outer = JSONArray.build {
            add(BigDecimal("1.5"))
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getInt(0) }.let {
            expect(JSON.TargetType.INT) { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.5")) { it.value }
            expect("Not an int (0) - 1.5") { it.message }
        }
    }

    @Test fun `should get short from object`() {
        val outer = JSONObject.build {
            add("mini", 12345)
        }
        expect(12345) { outer.getShort("mini") }
    }

    @Test fun `should fail on getShort from object when not a short`() {
        val outer = JSONObject.build {
            add("mini", 123456)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getShort("mini") }.let {
            expect(JSON.TargetType.SHORT) { it.target }
            expect("mini") { it.key }
            expect(JSONInt(123456)) { it.value }
            expect("Not a short (mini) - 123456") { it.message }
        }
    }

    @Test fun `should get short from array`() {
        val outer = JSONArray.build {
            add(-20000)
        }
        expect(-20000) { outer.getShort(0) }
    }

    @Test fun `should fail on getShort from array when not a short`() {
        val outer = JSONArray.build {
            add(false)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getShort(0) }.let {
            expect(JSON.TargetType.SHORT) { it.target }
            expect(0) { it.key }
            expect(JSONBoolean.FALSE) { it.value }
            expect("Not a short (0) - false") { it.message }
        }
    }

    @Test fun `should get byte from object`() {
        val outer = JSONObject.build {
            add("little", 123)
        }
        expect(123) { outer.getByte("little") }
    }

    @Test fun `should fail on getByte from object when not a byte`() {
        val outer = JSONObject.build {
            add("little", "why?")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getByte("little") }.let {
            expect(JSON.TargetType.BYTE) { it.target }
            expect("little") { it.key }
            expect(JSONString("why?")) { it.value }
            expect("Not a byte (little) - \"why?\"") { it.message }
        }
    }

    @Test fun `should get byte from array`() {
        val outer = JSONArray.build {
            add(-99)
        }
        expect(-99) { outer.getByte(0) }
    }

    @Test fun `should fail on getByte from array when not a byte`() {
        val outer = JSONArray.build {
            add("mushroom")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getByte(0) }.let {
            expect(JSON.TargetType.BYTE) { it.target }
            expect(0) { it.key }
            expect(JSONString("mushroom")) { it.value }
            expect("Not a byte (0) - \"mushroom\"") { it.message }
        }
    }

    @Test fun `should get ULong from object`() {
        val outer = JSONObject.build {
            add("big", 123456789123456789)
        }
        expect(123456789123456789U) { outer.getULong("big") }
    }

    @Test fun `should fail on getULong from object when not a ULong`() {
        val outer = JSONObject.build {
            add("big", true)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getULong("big") }.let {
            expect(JSON.TargetType.ULONG) { it.target }
            expect("big") { it.key }
            expect(JSONBoolean.TRUE) { it.value }
            expect("Not an unsigned long (big) - true") { it.message }
        }
    }

    @Test fun `should get ULong from array`() {
        val outer = JSONArray.build {
            add(99)
        }
        expect(99U) { outer.getULong(0) }
    }

    @Test fun `should fail on getULong from array when not a ULong`() {
        val outer = JSONArray.build {
            add(BigDecimal("1.1"))
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getULong(0) }.let {
            expect(JSON.TargetType.ULONG) { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.1")) { it.value }
            expect("Not an unsigned long (0) - 1.1") { it.message }
        }
    }

    @Test fun `should get UInt from object`() {
        val outer = JSONObject.build {
            add("number", 123456)
        }
        expect(123456U) { outer.getUInt("number") }
    }

    @Test fun `should fail on getUInt from object when not a UInt`() {
        val outer = JSONObject.build {
            add("number", -123456)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUInt("number") }.let {
            expect(JSON.TargetType.UINT) { it.target }
            expect("number") { it.key }
            expect(JSONInt(-123456)) { it.value }
            expect("Not an unsigned int (number) - -123456") { it.message }
        }
    }

    @Test fun `should get UInt from array`() {
        val outer = JSONArray.build {
            add(99)
        }
        expect(99U) { outer.getUInt(0) }
    }

    @Test fun `should fail on getUInt from array when not a UInt`() {
        val outer = JSONArray.build {
            add("incorrect")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUInt(0) }.let {
            expect(JSON.TargetType.UINT) { it.target }
            expect(0) { it.key }
            expect(JSONString("incorrect")) { it.value }
            expect("Not an unsigned int (0) - \"incorrect\"") { it.message }
        }
    }

    @Test fun `should get UShort from object`() {
        val outer = JSONObject.build {
            add("unit", 60000)
        }
        expect(60000U) { outer.getUShort("unit") }
    }

    @Test fun `should fail on getUShort from object when not a UShort`() {
        val outer = JSONObject.build {
            add("unit", -1)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUShort("unit") }.let {
            expect(JSON.TargetType.USHORT) { it.target }
            expect("unit") { it.key }
            expect(JSONInt(-1)) { it.value }
            expect("Not an unsigned short (unit) - -1") { it.message }
        }
    }

    @Test fun `should get UShort from array`() {
        val outer = JSONArray.build {
            add(1000)
        }
        expect(1000U) { outer.getUShort(0) }
    }

    @Test fun `should fail on getUShort from array when not a UShort`() {
        val outer = JSONArray.build {
            add(123456)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUShort(0) }.let {
            expect(JSON.TargetType.USHORT) { it.target }
            expect(0) { it.key }
            expect(JSONInt(123456)) { it.value }
            expect("Not an unsigned short (0) - 123456") { it.message }
        }
    }

    @Test fun `should get UByte from object`() {
        val outer = JSONObject.build {
            add("unit", 200)
        }
        expect(200U) { outer.getUByte("unit") }
    }

    @Test fun `should fail on getUByte from object when not a UByte`() {
        val outer = JSONObject.build {
            add("unit", -200)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUByte("unit") }.let {
            expect(JSON.TargetType.UBYTE) { it.target }
            expect("unit") { it.key }
            expect(JSONInt(-200)) { it.value }
            expect("Not an unsigned byte (unit) - -200") { it.message }
        }
    }

    @Test fun `should get UByte from array`() {
        val outer = JSONArray.build {
            add(150)
        }
        expect(150U) { outer.getUByte(0) }
    }

    @Test fun `should fail on getUByte from array when not a UByte`() {
        val outer = JSONArray.build {
            add(300)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getUByte(0) }.let {
            expect(JSON.TargetType.UBYTE) { it.target }
            expect(0) { it.key }
            expect(JSONInt(300)) { it.value }
            expect("Not an unsigned byte (0) - 300") { it.message }
        }
    }

    @Test fun `should get decimal from object`() {
        val outer = JSONObject.build {
            add("money", BigDecimal("2.50"))
        }
        expect(BigDecimal("2.50")) { outer.getDecimal("money") }
    }

    @Test fun `should fail on getDecimal from object when not a decimal`() {
        val outer = JSONObject.build {
            add("money", "bad")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getDecimal("money") }.let {
            expect(JSON.TargetType.DECIMAL) { it.target }
            expect("money") { it.key }
            expect(JSONString("bad")) { it.value }
            expect("Not a decimal (money) - \"bad\"") { it.message }
        }
    }

    @Test fun `should get decimal from array`() {
        val outer = JSONArray.build {
            add(BigDecimal("250.00"))
        }
        expect(BigDecimal("250.00")) { outer.getDecimal(0) }
    }

    @Test fun `should fail on getDecimal from array when not a decimal`() {
        val outer = JSONArray.build {
            add("sideboard")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getDecimal(0) }.let {
            expect(JSON.TargetType.DECIMAL) { it.target }
            expect(0) { it.key }
            expect(JSONString("sideboard")) { it.value }
            expect("Not a decimal (0) - \"sideboard\"") { it.message }
        }
    }

    @Test fun `should get boolean from object`() {
        val outer = JSONObject.build {
            add("bool1", false)
            add("bool2", true)
        }
        assertFalse { outer.getBoolean("bool1") }
        assertTrue { outer.getBoolean("bool2") }
    }

    @Test fun `should fail on getBoolean from object when not a boolean`() {
        val outer = JSONObject.build {
            add("bool1", 27)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getBoolean("bool1") }.let {
            expect(JSON.TargetType.BOOLEAN) { it.target }
            expect("bool1") { it.key }
            expect(JSONInt(27)) { it.value }
            expect("Not a boolean (bool1) - 27") { it.message }
        }
    }

    @Test fun `should get boolean from array`() {
        val outer = JSONArray.build {
            add(false)
            add(true)
        }
        assertFalse { outer.getBoolean(0) }
        assertTrue { outer.getBoolean(1) }
    }

    @Test fun `should fail on getBoolean from array when not a boolean`() {
        val outer = JSONArray.build {
            add("wrong")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getBoolean(0) }.let {
            expect(JSON.TargetType.BOOLEAN) { it.target }
            expect(0) { it.key }
            expect(JSONString("wrong")) { it.value }
            expect("Not a boolean (0) - \"wrong\"") { it.message }
        }
    }

    @Test fun `should get array from object`() {
        val outer = JSONObject.build {
            add("inner", JSONArray.build {
                add("ABC")
            })
        }
        val inner = outer.getArray("inner")
        expect("ABC") { inner[0].asString }
    }

    @Test fun `should fail on getArray from object when not an array`() {
        val outer = JSONObject.build {
            add("inner", 123)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getArray("inner") }.let {
            expect(JSON.TargetType.ARRAY) { it.target }
            expect("inner") { it.key }
            expect(JSONInt(123)) { it.value }
            expect("Not an array (inner) - 123") { it.message }
        }
    }

    @Test fun `should get array from array`() {
        val outer = JSONArray.build {
            add(JSONArray.build {
                add("ABC")
            })
        }
        val inner = outer.getArray(0)
        expect("ABC") { inner[0].asString }
    }

    @Test fun `should fail on getArray from array when not an array`() {
        val outer = JSONArray.build {
            add(false)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getArray(0) }.let {
            expect(JSON.TargetType.ARRAY) { it.target }
            expect(0) { it.key }
            expect(JSONBoolean.FALSE) { it.value }
            expect("Not an array (0) - false") { it.message }
        }
    }

    @Test fun `should get object from object`() {
        val outer = JSONObject.build {
            add("inner", JSONObject.build {
                add("item", "ABC")
            })
        }
        val inner = outer.getObject("inner")
        expect("ABC") { inner["item"].asString }
    }

    @Test fun `should fail on getObject from object when not an object`() {
        val outer = JSONObject.build {
            add("inner", 42)
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getObject("inner") }.let {
            expect(JSON.TargetType.OBJECT) { it.target }
            expect("inner") { it.key }
            expect(JSONInt(42)) { it.value }
            expect("Not an object (inner) - 42") { it.message }
        }
    }

    @Test fun `should get object from array`() {
        val outer = JSONArray.build {
            add(JSONObject.build {
                add("item", "ABC")
            })
        }
        val inner = outer.getObject(0)
        expect("ABC") { inner["item"].asString }
    }

    @Test fun `should fail on getObject from array when not an object`() {
        val outer = JSONArray.build {
            add("wrong")
        }
        assertFailsWith<JSONIncorrectTypeException> { outer.getObject(0) }.let {
            expect(JSON.TargetType.OBJECT) { it.target }
            expect(0) { it.key }
            expect(JSONString("wrong")) { it.value }
            expect("Not an object (0) - \"wrong\"") { it.message }
        }
    }

}
