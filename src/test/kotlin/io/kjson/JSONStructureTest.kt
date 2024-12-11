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

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldThrow

import io.kjson.JSON.asString

class JSONStructureTest {

    @Test fun `should get string from object`() {
        val outer = JSONObject.build {
            add("data", "something")
        }
        outer.getString("data") shouldBe "something"
    }

    @Test fun `should fail on getString from object when not a string`() {
        val outer = JSONObject.build {
            add("data", true)
        }
        shouldThrow<JSONTypeException>("Node not correct type (String), was true, at data") {
            outer.getString("data")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "String"
            it.key shouldBe "data"
            it.value shouldBe JSONBoolean.TRUE
        }
    }

    @Test fun `should get string from array`() {
        val outer = JSONArray.build {
            add("nice")
        }
        outer.getString(0) shouldBe "nice"
    }

    @Test fun `should fail on getString from array when not a string`() {
        val outer = JSONArray.build {
            add(42)
        }
        shouldThrow<JSONTypeException>("Node not correct type (String), was 42, at 0") {
            outer.getString(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "String"
            it.key shouldBe 0
            it.value shouldBe JSONInt(42)
        }
    }

    @Test fun `should get long from object`() {
        val outer = JSONObject.build {
            add("lots", 1234567812345678)
        }
        outer.getLong("lots") shouldBe 1234567812345678
    }

    @Test fun `should fail on getLong from object when not a long`() {
        val outer = JSONObject.build {
            add("lots", "millions")
        }
        shouldThrow<JSONTypeException>("Node not correct type (Long), was \"millions\", at lots") {
            outer.getLong("lots")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Long"
            it.key shouldBe "lots"
            it.value shouldBe JSONString("millions")
        }
    }

    @Test fun `should get long from array`() {
        val outer = JSONArray.build {
            add(-2244668822446688)
        }
        outer.getLong(0) shouldBe -2244668822446688
    }

    @Test fun `should fail on getLong from array when not a long`() {
        val outer = JSONArray.build {
            add("1.555".toBigDecimal())
        }
        shouldThrow<JSONTypeException>("Node not correct type (Long), was 1.555, at 0") {
            outer.getLong(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Long"
            it.key shouldBe 0
            it.value shouldBe JSONDecimal("1.555")
        }
    }

    @Test fun `should get int from object`() {
        val outer = JSONObject.build {
            add("number", 12345678)
        }
        outer.getInt("number") shouldBe 12345678
    }

    @Test fun `should fail on getInt from object when not an int`() {
        val outer = JSONObject.build {
            add("number", "trouble")
        }
        shouldThrow<JSONTypeException>("Node not correct type (Int), was \"trouble\", at number") {
            outer.getInt("number")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Int"
            it.key shouldBe "number"
            it.value shouldBe JSONString("trouble")
        }
    }

    @Test fun `should get int from array`() {
        val outer = JSONArray.build {
            add(-22446688)
        }
        outer.getInt(0) shouldBe -22446688
    }

    @Test fun `should fail on getInt from array when not an int`() {
        val outer = JSONArray.build {
            add("1.5".toBigDecimal())
        }
        shouldThrow<JSONTypeException>("Node not correct type (Int), was 1.5, at 0") {
            outer.getInt(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Int"
            it.key shouldBe 0
            it.value shouldBe JSONDecimal("1.5")
        }
    }

    @Test fun `should get short from object`() {
        val outer = JSONObject.build {
            add("mini", 12345)
        }
        outer.getShort("mini") shouldBe 12345
    }

    @Test fun `should fail on getShort from object when not a short`() {
        val outer = JSONObject.build {
            add("mini", 123456)
        }
        shouldThrow<JSONTypeException>("Node not correct type (Short), was 123456, at mini") {
            outer.getShort("mini")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Short"
            it.key shouldBe "mini"
            it.value shouldBe JSONInt(123456)
        }
    }

    @Test fun `should get short from array`() {
        val outer = JSONArray.build {
            add(-20000)
        }
        outer.getShort(0) shouldBe -20000
    }

    @Test fun `should fail on getShort from array when not a short`() {
        val outer = JSONArray.build {
            add(false)
        }
        shouldThrow<JSONTypeException>("Node not correct type (Short), was false, at 0") {
            outer.getShort(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Short"
            it.key shouldBe 0
            it.value shouldBe JSONBoolean.FALSE
        }
    }

    @Test fun `should get byte from object`() {
        val outer = JSONObject.build {
            add("little", 123)
        }
        outer.getByte("little") shouldBe 123
    }

    @Test fun `should fail on getByte from object when not a byte`() {
        val outer = JSONObject.build {
            add("little", "why?")
        }
        shouldThrow<JSONTypeException>("Node not correct type (Byte), was \"why?\", at little") {
            outer.getByte("little")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Byte"
            it.key shouldBe "little"
            it.value shouldBe JSONString("why?")
        }
    }

    @Test fun `should get byte from array`() {
        val outer = JSONArray.build {
            add(-99)
        }
        outer.getByte(0) shouldBe -99
    }

    @Test fun `should fail on getByte from array when not a byte`() {
        val outer = JSONArray.build {
            add("mushroom")
        }
        shouldThrow<JSONTypeException>("Node not correct type (Byte), was \"mushroom\", at 0") {
            outer.getByte(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Byte"
            it.key shouldBe 0
            it.value shouldBe JSONString("mushroom")
        }
    }

    @Test fun `should get ULong from object`() {
        val outer = JSONObject.build {
            add("big", 123456789123456789)
        }
        outer.getULong("big") shouldBe 123456789123456789U
    }

    @Test fun `should fail on getULong from object when not a ULong`() {
        val outer = JSONObject.build {
            add("big", true)
        }
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was true, at big") {
            outer.getULong("big")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe "big"
            it.value shouldBe JSONBoolean.TRUE
        }
    }

    @Test fun `should get ULong from array`() {
        val outer = JSONArray.build {
            add(99)
        }
        outer.getULong(0) shouldBe 99U
    }

    @Test fun `should fail on getULong from array when not a ULong`() {
        val outer = JSONArray.build {
            add("1.1".toBigDecimal())
        }
        shouldThrow<JSONTypeException>("Node not correct type (ULong), was 1.1, at 0") {
            outer.getULong(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "ULong"
            it.key shouldBe 0
            it.value shouldBe JSONDecimal("1.1")
        }
    }

    @Test fun `should get UInt from object`() {
        val outer = JSONObject.build {
            add("number", 123456)
        }
        outer.getUInt("number") shouldBe 123456U
    }

    @Test fun `should fail on getUInt from object when not a UInt`() {
        val outer = JSONObject.build {
            add("number", -123456)
        }
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was -123456, at number") {
            outer.getUInt("number")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe "number"
            it.value shouldBe JSONInt(-123456)
        }
    }

    @Test fun `should get UInt from array`() {
        val outer = JSONArray.build {
            add(99)
        }
        outer.getUInt(0) shouldBe 99U
    }

    @Test fun `should fail on getUInt from array when not a UInt`() {
        val outer = JSONArray.build {
            add("incorrect")
        }
        shouldThrow<JSONTypeException>("Node not correct type (UInt), was \"incorrect\", at 0") {
            outer.getUInt(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UInt"
            it.key shouldBe 0
            it.value shouldBe JSONString("incorrect")
        }
    }

    @Test fun `should get UShort from object`() {
        val outer = JSONObject.build {
            add("unit", 60000)
        }
        outer.getUShort("unit") shouldBe 60000U
    }

    @Test fun `should fail on getUShort from object when not a UShort`() {
        val outer = JSONObject.build {
            add("unit", -1)
        }
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was -1, at unit") {
            outer.getUShort("unit")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe "unit"
            it.value shouldBe JSONInt(-1)
        }
    }

    @Test fun `should get UShort from array`() {
        val outer = JSONArray.build {
            add(1000)
        }
        outer.getUShort(0) shouldBe 1000U
    }

    @Test fun `should fail on getUShort from array when not a UShort`() {
        val outer = JSONArray.build {
            add(123456)
        }
        shouldThrow<JSONTypeException>("Node not correct type (UShort), was 123456, at 0") {
            outer.getUShort(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UShort"
            it.key shouldBe 0
            it.value shouldBe JSONInt(123456)
        }
    }

    @Test fun `should get UByte from object`() {
        val outer = JSONObject.build {
            add("unit", 200)
        }
        outer.getUByte("unit") shouldBe 200U
    }

    @Test fun `should fail on getUByte from object when not a UByte`() {
        val outer = JSONObject.build {
            add("unit", -200)
        }
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was -200, at unit") {
            outer.getUByte("unit")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe "unit"
            it.value shouldBe JSONInt(-200)
        }
    }

    @Test fun `should get UByte from array`() {
        val outer = JSONArray.build {
            add(150)
        }
        outer.getUByte(0) shouldBe 150U
    }

    @Test fun `should fail on getUByte from array when not a UByte`() {
        val outer = JSONArray.build {
            add(300)
        }
        shouldThrow<JSONTypeException>("Node not correct type (UByte), was 300, at 0") {
            outer.getUByte(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "UByte"
            it.key shouldBe 0
            it.value shouldBe JSONInt(300)
        }
    }

    @Test fun `should get decimal from object`() {
        val outer = JSONObject.build {
            add("money", "2.50".toBigDecimal())
        }
        outer.getDecimal("money") shouldBe "2.50".toBigDecimal()
    }

    @Test fun `should fail on getDecimal from object when not a decimal`() {
        val outer = JSONObject.build {
            add("money", "bad")
        }
        shouldThrow<JSONTypeException>("Node not correct type (BigDecimal), was \"bad\", at money") {
            outer.getDecimal("money")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "BigDecimal"
            it.key shouldBe "money"
            it.value shouldBe JSONString("bad")
        }
    }

    @Test fun `should get decimal from array`() {
        val outer = JSONArray.build {
            add("250.00".toBigDecimal())
        }
        outer.getDecimal(0) shouldBe "250.00".toBigDecimal()
    }

    @Test fun `should fail on getDecimal from array when not a decimal`() {
        val outer = JSONArray.build {
            add("sideboard")
        }
        shouldThrow<JSONTypeException>("Node not correct type (BigDecimal), was \"sideboard\", at 0") {
            outer.getDecimal(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "BigDecimal"
            it.key shouldBe 0
            it.value shouldBe JSONString("sideboard")
        }
    }

    @Test fun `should get boolean from object`() {
        val outer = JSONObject.build {
            add("bool1", false)
            add("bool2", true)
        }
        outer.getBoolean("bool1") shouldBe false
        outer.getBoolean("bool2") shouldBe true
    }

    @Test fun `should fail on getBoolean from object when not a boolean`() {
        val outer = JSONObject.build {
            add("bool1", 27)
        }
        shouldThrow<JSONTypeException>("Node not correct type (Boolean), was 27, at bool1") {
            outer.getBoolean("bool1")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Boolean"
            it.key shouldBe "bool1"
            it.value shouldBe JSONInt(27)
        }
    }

    @Test fun `should get boolean from array`() {
        val outer = JSONArray.build {
            add(false)
            add(true)
        }
        outer.getBoolean(0) shouldBe false
        outer.getBoolean(1) shouldBe true
    }

    @Test fun `should fail on getBoolean from array when not a boolean`() {
        val outer = JSONArray.build {
            add("wrong")
        }
        shouldThrow<JSONTypeException>("Node not correct type (Boolean), was \"wrong\", at 0") {
            outer.getBoolean(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "Boolean"
            it.key shouldBe 0
            it.value shouldBe JSONString("wrong")
        }
    }

    @Test fun `should get array from object`() {
        val outer = JSONObject.build {
            add("inner", JSONArray.build {
                add("ABC")
            })
        }
        val inner = outer.getArray("inner")
        inner[0].asString shouldBe "ABC"
    }

    @Test fun `should fail on getArray from object when not an array`() {
        val outer = JSONObject.build {
            add("inner", 123)
        }
        shouldThrow<JSONTypeException>("Node not correct type (JSONArray), was 123, at inner") {
            outer.getArray("inner")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONArray"
            it.key shouldBe "inner"
            it.value shouldBe JSONInt(123)
        }
    }

    @Test fun `should get array from array`() {
        val outer = JSONArray.build {
            add(JSONArray.build {
                add("ABC")
            })
        }
        val inner = outer.getArray(0)
        inner[0].asString shouldBe "ABC"
    }

    @Test fun `should fail on getArray from array when not an array`() {
        val outer = JSONArray.build {
            add(false)
        }
        shouldThrow<JSONTypeException>("Node not correct type (JSONArray), was false, at 0") {
            outer.getArray(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONArray"
            it.key shouldBe 0
            it.value shouldBe JSONBoolean.FALSE
        }
    }

    @Test fun `should get object from object`() {
        val outer = JSONObject.build {
            add("inner", JSONObject.build {
                add("item", "ABC")
            })
        }
        val inner = outer.getObject("inner")
        inner["item"].asString shouldBe "ABC"
    }

    @Test fun `should fail on getObject from object when not an object`() {
        val outer = JSONObject.build {
            add("inner", 42)
        }
        shouldThrow<JSONTypeException>("Node not correct type (JSONObject), was 42, at inner") {
            outer.getObject("inner")
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONObject"
            it.key shouldBe "inner"
            it.value shouldBe JSONInt(42)
        }
    }

    @Test fun `should get object from array`() {
        val outer = JSONArray.build {
            add(JSONObject.build {
                add("item", "ABC")
            })
        }
        val inner = outer.getObject(0)
        inner["item"].asString shouldBe "ABC"
    }

    @Test fun `should fail on getObject from array when not an object`() {
        val outer = JSONArray.build {
            add("wrong")
        }
        shouldThrow<JSONTypeException>("Node not correct type (JSONObject), was \"wrong\", at 0") {
            outer.getObject(0)
        }.let {
            it.nodeName shouldBe "Node"
            it.expected shouldBe "JSONObject"
            it.key shouldBe 0
            it.value shouldBe JSONString("wrong")
        }
    }

}
