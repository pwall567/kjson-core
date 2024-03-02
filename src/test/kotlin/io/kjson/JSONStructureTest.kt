/*
 * @(#) JSONStructureTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2022, 2024 Peter Wall
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
        assertFailsWith<JSONTypeException> { outer.getString("data") }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            expect("data") { it.key }
            expect(JSONBoolean.TRUE) { it.value }
            expect("Node not correct type (String), was true, at data") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getString(0) }.let {
            expect("Node") { it.nodeName }
            expect("String") { it.target }
            expect(0) { it.key }
            expect(JSONInt(42)) { it.value }
            expect("Node not correct type (String), was 42, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getLong("lots") }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            expect("lots") { it.key }
            expect(JSONString("millions")) { it.value }
            expect("Node not correct type (Long), was \"millions\", at lots") { it.message }
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
            add("1.555".toBigDecimal())
        }
        assertFailsWith<JSONTypeException> { outer.getLong(0) }.let {
            expect("Node") { it.nodeName }
            expect("Long") { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.555")) { it.value }
            expect("Node not correct type (Long), was 1.555, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getInt("number") }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            expect("number") { it.key }
            expect(JSONString("trouble")) { it.value }
            expect("Node not correct type (Int), was \"trouble\", at number") { it.message }
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
            add("1.5".toBigDecimal())
        }
        assertFailsWith<JSONTypeException> { outer.getInt(0) }.let {
            expect("Node") { it.nodeName }
            expect("Int") { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.5")) { it.value }
            expect("Node not correct type (Int), was 1.5, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getShort("mini") }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            expect("mini") { it.key }
            expect(JSONInt(123456)) { it.value }
            expect("Node not correct type (Short), was 123456, at mini") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getShort(0) }.let {
            expect("Node") { it.nodeName }
            expect("Short") { it.target }
            expect(0) { it.key }
            expect(JSONBoolean.FALSE) { it.value }
            expect("Node not correct type (Short), was false, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getByte("little") }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            expect("little") { it.key }
            expect(JSONString("why?")) { it.value }
            expect("Node not correct type (Byte), was \"why?\", at little") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getByte(0) }.let {
            expect("Node") { it.nodeName }
            expect("Byte") { it.target }
            expect(0) { it.key }
            expect(JSONString("mushroom")) { it.value }
            expect("Node not correct type (Byte), was \"mushroom\", at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getULong("big") }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            expect("big") { it.key }
            expect(JSONBoolean.TRUE) { it.value }
            expect("Node not correct type (ULong), was true, at big") { it.message }
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
            add("1.1".toBigDecimal())
        }
        assertFailsWith<JSONTypeException> { outer.getULong(0) }.let {
            expect("Node") { it.nodeName }
            expect("ULong") { it.target }
            expect(0) { it.key }
            expect(JSONDecimal("1.1")) { it.value }
            expect("Node not correct type (ULong), was 1.1, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUInt("number") }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            expect("number") { it.key }
            expect(JSONInt(-123456)) { it.value }
            expect("Node not correct type (UInt), was -123456, at number") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUInt(0) }.let {
            expect("Node") { it.nodeName }
            expect("UInt") { it.target }
            expect(0) { it.key }
            expect(JSONString("incorrect")) { it.value }
            expect("Node not correct type (UInt), was \"incorrect\", at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUShort("unit") }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            expect("unit") { it.key }
            expect(JSONInt(-1)) { it.value }
            expect("Node not correct type (UShort), was -1, at unit") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUShort(0) }.let {
            expect("Node") { it.nodeName }
            expect("UShort") { it.target }
            expect(0) { it.key }
            expect(JSONInt(123456)) { it.value }
            expect("Node not correct type (UShort), was 123456, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUByte("unit") }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            expect("unit") { it.key }
            expect(JSONInt(-200)) { it.value }
            expect("Node not correct type (UByte), was -200, at unit") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getUByte(0) }.let {
            expect("Node") { it.nodeName }
            expect("UByte") { it.target }
            expect(0) { it.key }
            expect(JSONInt(300)) { it.value }
            expect("Node not correct type (UByte), was 300, at 0") { it.message }
        }
    }

    @Test fun `should get decimal from object`() {
        val outer = JSONObject.build {
            add("money", "2.50".toBigDecimal())
        }
        expect("2.50".toBigDecimal()) { outer.getDecimal("money") }
    }

    @Test fun `should fail on getDecimal from object when not a decimal`() {
        val outer = JSONObject.build {
            add("money", "bad")
        }
        assertFailsWith<JSONTypeException> { outer.getDecimal("money") }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            expect("money") { it.key }
            expect(JSONString("bad")) { it.value }
            expect("Node not correct type (BigDecimal), was \"bad\", at money") { it.message }
        }
    }

    @Test fun `should get decimal from array`() {
        val outer = JSONArray.build {
            add("250.00".toBigDecimal())
        }
        expect("250.00".toBigDecimal()) { outer.getDecimal(0) }
    }

    @Test fun `should fail on getDecimal from array when not a decimal`() {
        val outer = JSONArray.build {
            add("sideboard")
        }
        assertFailsWith<JSONTypeException> { outer.getDecimal(0) }.let {
            expect("Node") { it.nodeName }
            expect("BigDecimal") { it.target }
            expect(0) { it.key }
            expect(JSONString("sideboard")) { it.value }
            expect("Node not correct type (BigDecimal), was \"sideboard\", at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getBoolean("bool1") }.let {
            expect("Node") { it.nodeName }
            expect("Boolean") { it.target }
            expect("bool1") { it.key }
            expect(JSONInt(27)) { it.value }
            expect("Node not correct type (Boolean), was 27, at bool1") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getBoolean(0) }.let {
            expect("Node") { it.nodeName }
            expect("Boolean") { it.target }
            expect(0) { it.key }
            expect(JSONString("wrong")) { it.value }
            expect("Node not correct type (Boolean), was \"wrong\", at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getArray("inner") }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            expect("inner") { it.key }
            expect(JSONInt(123)) { it.value }
            expect("Node not correct type (JSONArray), was 123, at inner") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getArray(0) }.let {
            expect("Node") { it.nodeName }
            expect("JSONArray") { it.target }
            expect(0) { it.key }
            expect(JSONBoolean.FALSE) { it.value }
            expect("Node not correct type (JSONArray), was false, at 0") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getObject("inner") }.let {
            expect("Node") { it.nodeName }
            expect("JSONObject") { it.target }
            expect("inner") { it.key }
            expect(JSONInt(42)) { it.value }
            expect("Node not correct type (JSONObject), was 42, at inner") { it.message }
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
        assertFailsWith<JSONTypeException> { outer.getObject(0) }.let {
            expect("JSONObject") { it.target }
            expect(0) { it.key }
            expect(JSONString("wrong")) { it.value }
            expect("Node not correct type (JSONObject), was \"wrong\", at 0") { it.message }
        }
    }

}
