/*
 * @(#) JSONArrayTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024 Peter Wall
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

import kotlinx.coroutines.runBlocking

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldThrow

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONArrayTest {

    @Test fun `should create JSONArray`() {
        val testArray = JSONArray(arrayOf(JSONInt(123), JSONInt(456)), 2)
        testArray.size shouldBe 2
        testArray[0] shouldBe JSONInt(123)
        testArray[1] shouldBe JSONInt(456)
        testArray.toString() shouldBe "[123,456]"
        testArray.toJSON() shouldBe "[123,456]"
        testArray.isEmpty() shouldBe false
        testArray.isNotEmpty() shouldBe true
    }

    @Test fun `should create empty JSONArray`() {
        val testArray = JSONArray(emptyArray(), 0)
        testArray.size shouldBe 0
        testArray.toString() shouldBe "[]"
        testArray.toJSON() shouldBe "[]"
        testArray.isEmpty() shouldBe true
        testArray.isNotEmpty() shouldBe false
    }

    @Test fun `should create JSONArray using of`() {
        val testArray = JSONArray.of(JSONInt(9999), JSONInt(8888))
        testArray.size shouldBe 2
        testArray[0] shouldBe JSONInt(9999)
        testArray[1] shouldBe JSONInt(8888)
        testArray.toJSON() shouldBe "[9999,8888]"
    }

    @Test fun `should create JSONArray using JSONArray function`() {
        val testArray = JSONArray(JSONInt(9999), JSONInt(8888))
        testArray.size shouldBe 2
        testArray[0] shouldBe JSONInt(9999)
        testArray[1] shouldBe JSONInt(8888)
        testArray.toJSON() shouldBe "[9999,8888]"
    }

    @Test fun `should create JSONArray using List`() {
        val testArray = JSONArray.from(listOf(JSONString("Hello"), JSONString("World")))
        testArray.size shouldBe 2
        testArray[0] shouldBe JSONString("Hello")
        testArray[1] shouldBe JSONString("World")
        testArray.toJSON() shouldBe "[\"Hello\",\"World\"]"
    }

    @Test fun `should create JSONArray using List extension function`() {
        val testArray = listOf(JSONString("Hello"), JSONString("World")).toJSONArray()
        testArray.size shouldBe 2
        testArray[0] shouldBe JSONString("Hello")
        testArray[1] shouldBe JSONString("World")
        testArray.toJSON() shouldBe "[\"Hello\",\"World\"]"
    }

    @Test fun `should compare to other List`() {
        val list = listOf(JSONInt(123), JSONInt(456))
        val testArray = JSONArray.from(list)
        testArray.size shouldBe 2
        list.shouldBe<List<*>>(testArray)
        testArray.shouldBe<List<*>>(list)
        list.hashCode() shouldBe testArray.hashCode()
    }

    @Test fun `should build JSONArray using Builder`() {
        val json = JSONArray.Builder {
            add(JSONInt(123))
            add(JSONInt(456))
            add(JSONInt(789))
        }.build()
        json.size shouldBe 3
        json[0] shouldBe JSONInt(123)
        json[1] shouldBe JSONInt(456)
        json[2] shouldBe JSONInt(789)
    }

    @Test fun `should build JSONArray using build`() {
        val json = JSONArray.build {
            add(JSONInt(123))
            add(JSONInt(456))
            add(JSONInt(789))
        }
        json.size shouldBe 3
        json[0] shouldBe JSONInt(123)
        json[1] shouldBe JSONInt(456)
        json[2] shouldBe JSONInt(789)
    }

    @Test fun `should build JSONArray using build with non-JSON classes`() {
        val json = JSONArray.build {
            add(123)
            add("abc")
            add(112233445566778899)
            add("1.456".toBigDecimal())
            add(true)
        }
        json.size shouldBe 5
        json[0] shouldBe JSONInt(123)
        json[1] shouldBe JSONString("abc")
        json[2] shouldBe JSONLong(112233445566778899)
        json[3] shouldBe JSONDecimal("1.456".toBigDecimal())
        json[4] shouldBe JSONBoolean.TRUE
    }

    @Test fun `should limit Builder to single use`() {
        val builder = JSONArray.Builder(2)
        builder.add(JSONInt(111))
        builder.add(JSONInt(222))
        builder.add(JSONInt(333))
        builder.size shouldBe 3
        val json = builder.build()
        json.size shouldBe 3
        json[0] shouldBe JSONInt(111)
        json[1] shouldBe JSONInt(222)
        json[2] shouldBe JSONInt(333)
        shouldThrow<JSONException>("Builder is closed") { builder.add(JSONInt(444)) }
    }

    @Test fun `should return JSONArray from subList`() {
        val array = JSONArray.of(JSONInt(111), JSONInt(222), JSONInt(333), JSONInt(444), JSONInt(555))
        val subList = array.subList(2, 4)
        subList.toJSON() shouldBe "[333,444]"
    }

    @Test fun `should format JSONArray using output`() {
        val capture = OutputCapture(64)
        JSONArray.from(listOf(JSONString("Hello"), JSONInt(123), JSONBoolean.TRUE,
            JSONDecimal("1.5".toBigDecimal()), null, JSONLong(0))).outputTo(capture)
        capture.toString() shouldBe "[\"Hello\",123,true,1.5,null,0]"
    }

    @Test fun `should format JSONArray using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONArray.from(listOf(JSONString("Hello"), JSONInt(123), JSONBoolean.TRUE,
            JSONDecimal("1.5".toBigDecimal()), null, JSONLong(0))).coOutputTo(capture)
        capture.toString() shouldBe "[\"Hello\",123,true,1.5,null,0]"
    }

    @Test fun `should iterate over array`() {
        val json = JSONArray.build {
            add(123)
            add("abc")
            add(112233445566778899)
            add("1.456".toBigDecimal())
            add(true)
        }
        var count = 0
        json.forEachItem {
            when (count++) {
                0 -> it shouldBe JSONInt(123)
                1 -> it shouldBe JSONString("abc")
                2 -> it shouldBe JSONLong(112233445566778899)
                3 -> it shouldBe JSONDecimal("1.456")
                4 -> it shouldBe JSONBoolean.TRUE
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over array including index`() {
        val json = JSONArray.build {
            add(123)
            add("abc")
            add(112233445566778899)
            add("1.456".toBigDecimal())
            add(true)
        }
        var count = 0
        json.forEachItemIndexed { index, item ->
            index shouldBe count
            when (count++) {
                0 -> item shouldBe JSONInt(123)
                1 -> item shouldBe JSONString("abc")
                2 -> item shouldBe JSONLong(112233445566778899)
                3 -> item shouldBe JSONDecimal("1.456")
                4 -> item shouldBe JSONBoolean.TRUE
            }
        }
        count shouldBe 5
    }

    @Test fun `should append in JSON Lines format`() {
        val json = createJSONLines()
        val sb = StringBuilder()
        json.appendJSONLinesTo(sb)
        sb.toString() shouldBe "{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n"
    }

    @Test fun `should append empty JSONArray in JSON Lines format`() {
        val sb = StringBuilder()
        JSONArray.EMPTY.appendJSONLinesTo(sb)
        sb.toString() shouldBe ""
    }

    @Test fun `should create JSON Lines string`() {
        val json = createJSONLines()
        json.toJSONLines() shouldBe "{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n"
    }

    @Test fun `should output empty string for toJSONLines of empty JSONArray`() {
        val json = JSONArray.EMPTY
        json.toJSONLines() shouldBe ""
    }

    @Test fun `should output JSON Lines using output`() {
        val capture = OutputCapture(64)
        createJSONLines().outputJSONLinesTo(capture)
        capture.toString() shouldBe "{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n"
    }

    @Test fun `should output JSON Lines using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        createJSONLines().coOutputJSONLinesTo(capture)
        capture.toString() shouldBe "{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n"
    }

    companion object {

        fun createJSONLines() = JSONArray.build {
            add(JSONObject.build {
                add("a", 1)
                add("b", 2)
            })
            add(JSONObject.build {
                add("a", 21)
                add("b", 34)
            })
            add(JSONObject.build {
                add("a", 55)
                add("b", 66)
            })
        }

    }

}
