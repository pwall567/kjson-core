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
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONArrayTest {

    @Test fun `should create JSONArray`() {
        val testArray = JSONArray(arrayOf(JSONInt(123), JSONInt(456)), 2)
        expect(2) { testArray.size }
        expect(JSONInt(123)) { testArray[0] }
        expect(JSONInt(456)) { testArray[1] }
        expect("[123,456]") { testArray.toString() }
        expect("[123,456]") { testArray.toJSON() }
        assertFalse(testArray.isEmpty())
        assertTrue(testArray.isNotEmpty())
    }

    @Test fun `should create empty JSONArray`() {
        val testArray = JSONArray(emptyArray(), 0)
        expect(0) { testArray.size }
        expect("[]") { testArray.toString() }
        expect("[]") { testArray.toJSON() }
        assertTrue(testArray.isEmpty())
        assertFalse(testArray.isNotEmpty())
    }

    @Test fun `should create JSONArray using of`() {
        val testArray = JSONArray.of(JSONInt(9999), JSONInt(8888))
        expect(2) { testArray.size }
        expect(JSONInt(9999)) { testArray[0] }
        expect(JSONInt(8888)) { testArray[1] }
        expect("[9999,8888]") { testArray.toJSON() }
    }

    @Test fun `should create JSONArray using List`() {
        val testArray = JSONArray.from(listOf(JSONString("Hello"), JSONString("World")))
        expect(2) { testArray.size }
        expect(JSONString("Hello")) { testArray[0] }
        expect(JSONString("World")) { testArray[1] }
        expect("[\"Hello\",\"World\"]") { testArray.toJSON() }
    }

    @Test fun `should compare to other List`() {
        val list = listOf(JSONInt(123), JSONInt(456))
        val testArray = JSONArray.from(list)
        expect(2) { testArray.size }
        assertEquals<List<*>>(testArray, list)
        assertEquals<List<*>>(list, testArray)
        assertEquals(testArray.hashCode(), list.hashCode())
    }

    @Test fun `should build JSONArray using Builder`() {
        val json = JSONArray.Builder {
            add(JSONInt(123))
            add(JSONInt(456))
            add(JSONInt(789))
        }.build()
        expect(3) { json.size }
        expect(JSONInt(123)) { json[0] }
        expect(JSONInt(456)) { json[1] }
        expect(JSONInt(789)) { json[2] }
    }

    @Test fun `should build JSONArray using build`() {
        val json = JSONArray.build {
            add(JSONInt(123))
            add(JSONInt(456))
            add(JSONInt(789))
        }
        expect(3) { json.size }
        expect(JSONInt(123)) { json[0] }
        expect(JSONInt(456)) { json[1] }
        expect(JSONInt(789)) { json[2] }
    }

    @Test fun `should build JSONArray using build with non-JSON classes`() {
        val json = JSONArray.build {
            add(123)
            add("abc")
            add(112233445566778899)
            add("1.456".toBigDecimal())
            add(true)
        }
        expect(5) { json.size }
        expect(JSONInt(123)) { json[0] }
        expect(JSONString("abc")) { json[1] }
        expect(JSONLong(112233445566778899)) { json[2] }
        expect(JSONDecimal("1.456".toBigDecimal())) { json[3] }
        expect(JSONBoolean.TRUE) { json[4] }
    }

    @Test fun `should limit Builder to single use`() {
        val builder = JSONArray.Builder(2)
        builder.add(JSONInt(111))
        builder.add(JSONInt(222))
        builder.add(JSONInt(333))
        expect(3) { builder.size }
        val json = builder.build()
        expect(3) { json.size }
        expect(JSONInt(111)) { json[0] }
        expect(JSONInt(222)) { json[1] }
        expect(JSONInt(333)) { json[2] }
        assertFailsWith<JSONException> { builder.add(JSONInt(444)) }.let {
            expect("Builder is closed") { it.message }
        }
    }

    @Test fun `should return JSONArray from subList`() {
        val array = JSONArray.of(JSONInt(111), JSONInt(222), JSONInt(333), JSONInt(444), JSONInt(555))
        val subList = array.subList(2, 4)
        expect("[333,444]") { subList.toJSON() }
    }

    @Test fun `should format JSONArray using output`() {
        val capture = OutputCapture(64)
        JSONArray.from(listOf(JSONString("Hello"), JSONInt(123), JSONBoolean.TRUE,
                JSONDecimal("1.5".toBigDecimal()), null, JSONLong(0))).output(capture)
        expect("[\"Hello\",123,true,1.5,null,0]") { capture.toString() }
    }

    @Test fun `should format JSONArray using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONArray.from(listOf(JSONString("Hello"), JSONInt(123), JSONBoolean.TRUE,
                JSONDecimal("1.5".toBigDecimal()), null, JSONLong(0))).coOutput(capture)
        expect("[\"Hello\",123,true,1.5,null,0]") { capture.toString() }
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
                0 -> expect(JSONInt(123)) { it }
                1 -> expect(JSONString("abc")) { it }
                2 -> expect(JSONLong(112233445566778899)) { it }
                3 -> expect(JSONDecimal("1.456")) { it }
                4 -> expect(JSONBoolean.TRUE) { it }
            }
        }
        expect(5) { count }
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
            expect(count) { index }
            when (count++) {
                0 -> expect(JSONInt(123)) { item }
                1 -> expect(JSONString("abc")) { item }
                2 -> expect(JSONLong(112233445566778899)) { item }
                3 -> expect(JSONDecimal("1.456")) { item }
                4 -> expect(JSONBoolean.TRUE) { item }
            }
        }
        expect(5) { count }
    }

    @Test fun `should append in JSON Lines format`() {
        val json = createJSONLines()
        val sb = StringBuilder()
        json.appendJSONLines(sb)
        expect("{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n") { sb.toString() }
    }

    @Test fun `should create JSON Lines string`() {
        val json = createJSONLines()
        expect("{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n") { json.toJSONLines() }
    }

    @Test fun `should output JSON Lines using output`() {
        val capture = OutputCapture(64)
        createJSONLines().outputJSONLines(capture)
        expect("{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n") { capture.toString() }
    }

    @Test fun `should output JSON Lines using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        createJSONLines().coOutputJSONLines(capture)
        expect("{\"a\":1,\"b\":2}\n{\"a\":21,\"b\":34}\n{\"a\":55,\"b\":66}\n") { capture.toString() }
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
