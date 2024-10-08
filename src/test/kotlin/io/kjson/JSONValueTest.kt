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
import kotlin.test.assertIs
import kotlin.test.expect

import java.math.BigDecimal

import io.kjson.JSON.appendJSONValue
import io.kjson.JSON.appendTo
import io.kjson.JSON.toJSON

class JSONValueTest {

    @Test fun `should use nullable functions`() {
        var testNull: JSONValue? = createJSONValueNull()
        expect("null") { testNull.toJSON() }
        val sb = StringBuilder()
        testNull.appendTo(sb)
        expect("null") { sb.toString() }
        testNull = createJSONValue()
        expect("222") { testNull.toJSON() }
        sb.setLength(0)
        testNull.appendTo(sb)
        expect("222") { sb.toString() }
    }

    @Test fun `should use Appendable appendJSON`() {
        val test = StringBuilder()
        test.appendJSONValue(JSONArray.of(JSONInt(123), JSONInt(321)))
        expect("[123,321]") { test.toString() }
    }

    @Test fun `should create JSONValue of correct type using of function`() {
        assertIs<JSONInt>(JSON.of(123))
        assertIs<JSONLong>(JSON.of(0L))
        assertIs<JSONDecimal>(JSON.of(BigDecimal.ONE))
        assertIs<JSONString>(JSON.of("hello"))
        assertIs<JSONBoolean>(JSON.of(true))
        assertIs<JSONArray>(JSON.of(JSONValue(0), JSONValue(1)))
        assertIs<JSONObject>(JSON.of("alpha" to JSONValue(0), "beta" to JSONValue(1)))
    }

    @Test fun `should create JSONValue of correct type using JSONValue function`() {
        assertIs<JSONInt>(JSONValue(123))
        assertIs<JSONLong>(JSONValue(0L))
        assertIs<JSONDecimal>(JSONValue(BigDecimal.ONE))
        assertIs<JSONString>(JSONValue("hello"))
        assertIs<JSONBoolean>(JSONValue(true))
        assertIs<JSONArray>(JSONValue(JSONValue(0), JSONValue(1)))
        assertIs<JSONObject>(JSONValue("alpha" to JSONValue(0), "beta" to JSONValue(1)))
        assertIs<JSONObject>(JSONValue("alpha" refersTo  JSONValue(0), "beta" refersTo  JSONValue(1)))
    }

    companion object {

        @Suppress("RedundantNullableReturnType")
        private fun createJSONValue(): JSONValue? = JSONInt(222)

        private fun createJSONValueNull(): JSONValue? = null

    }

}
