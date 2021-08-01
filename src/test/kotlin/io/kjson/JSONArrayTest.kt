/*
 * @(#) JSONArrayTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021 Peter Wall
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
import kotlin.test.expect

class JSONArrayTest {

    @Test fun `should create JSONArray`() {
        val testArray = JSONArray(arrayOf(JSONInt(123), JSONInt(456)), 2)
        expect(2) { testArray.size }
        expect(JSONInt(123)) { testArray[0] }
        expect(JSONInt(456)) { testArray[1] }
        expect("[123,456]") { testArray.toJSON() }
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
        val testArray = JSONArray.from(listOf(JSONInt(123), JSONInt(456)))
        expect(2) { testArray.size }
        assertEquals<List<*>>(testArray, listOf(JSONInt(123), JSONInt(456)))
        assertEquals<List<*>>(listOf(JSONInt(123), JSONInt(456)), testArray)
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

}
