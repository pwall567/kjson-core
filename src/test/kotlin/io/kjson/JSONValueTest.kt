/*
 * @(#) JSONValueTest.kt
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
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue
import kotlin.test.expect

import java.math.BigDecimal

import io.kjson.JSON.appendJSON
import io.kjson.JSON.appendTo
import io.kjson.JSON.toJSON

class JSONValueTest {

    @Test fun `should create values using JSON object`() {
        val testInt = JSON.of(54321)
        expect(JSONInt(54321)) { testInt }
        val testLong = JSON.of(2233445566778899)
        expect(JSONLong(2233445566778899)) { testLong }
        val testDecimal = JSON.of(BigDecimal("99.999"))
        expect(JSONDecimal(BigDecimal("99.999"))) { testDecimal }
        val testString = JSON.of("Hello!")
        expect(JSONString("Hello!")) { testString }
    }

    @Test fun `should parse using JSON object`() {
        val json = JSON.parse("""{"one":1,"two":2}""")
        assertTrue(json is JSONObject)
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
    }

    @Test fun `should parse using JSON object parseObject`() {
        val json = JSON.parseObject("""{"one":1,"two":2}""")
        expect(2) { json.size }
        expect(JSONInt(1)) { json["one"] }
        expect(JSONInt(2)) { json["two"] }
        assertFailsWith<JSONException> { JSON.parseObject("[1,2,3]") }.let {
            expect("JSON is not an object") { it.message }
        }
    }

    @Test fun `should parse using JSON object parseArray`() {
        val json = JSON.parseArray("""["alpha","beta","gamma"]""")
        expect(3) { json.size }
        expect(JSONString("alpha")) { json[0] }
        expect(JSONString("beta")) { json[1] }
        expect(JSONString("gamma")) { json[2] }
        assertFailsWith<JSONException> { JSON.parseObject("[1,2,3]") }.let {
            expect("JSON is not an object") { it.message }
        }
    }

    @Test fun `should use nullable functions`() {
        var testNull: JSONValue? = null
        expect("null") { testNull.toJSON() }
        val sb = StringBuilder()
        testNull.appendTo(sb)
        expect("null") { sb.toString() }
        testNull = createJSONValue()
        expect("222") { testNull.toJSON() }
        sb.setLength(0)
        testNull.appendTo(sb)
    }

    @Test fun `should use Appendable appendJSON`() {
        val test = StringBuilder()
        test.appendJSON(JSONArray.of(JSONInt(123), JSONInt(321)))
        expect("[123,321]") { test.toString() }
    }

    @Suppress("RedundantNullableReturnType")
    private fun createJSONValue(): JSONValue? {
        return JSONInt(222)
    }

}
