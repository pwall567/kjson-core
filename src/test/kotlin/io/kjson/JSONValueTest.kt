/*
 * @(#) JSONValueTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2024 Peter Wall
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

import java.math.BigDecimal

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeType

import io.kjson.JSON.appendJSONValue
import io.kjson.JSON.appendTo
import io.kjson.JSON.toJSON

class JSONValueTest {

    @Test fun `should use nullable functions`() {
        var testNull: JSONValue? = createJSONValueNull()
        testNull.toJSON() shouldBe "null"
        val sb = StringBuilder()
        testNull.appendTo(sb)
        sb.toString() shouldBe "null"
        testNull = createJSONValue()
        testNull.toJSON() shouldBe "222"
        sb.setLength(0)
        testNull.appendTo(sb)
        sb.toString() shouldBe "222"
    }

    @Test fun `should use Appendable appendJSON`() {
        val test = StringBuilder()
        test.appendJSONValue(JSONArray.of(JSONInt(123), JSONInt(321)))
        test.toString() shouldBe "[123,321]"
    }

    @Test fun `should create JSONValue of correct type using of function`() {
        JSON.of(123).shouldBeType<JSONInt>()
        JSON.of(0L).shouldBeType<JSONLong>()
        JSON.of(BigDecimal.ONE).shouldBeType<JSONDecimal>()
        JSON.of("hello").shouldBeType<JSONString>()
        JSON.of(true).shouldBeType<JSONBoolean>()
        JSON.of(JSONValue(0), JSONValue(1)).shouldBeType<JSONArray>()
        JSON.of("alpha" to JSONValue(0), "beta" to JSONValue(1)).shouldBeType<JSONObject>()
    }

    @Test fun `should create JSONValue of correct type using JSONValue function`() {
        JSONValue(123).shouldBeType<JSONInt>()
        JSONValue(0L).shouldBeType<JSONLong>()
        JSONValue(BigDecimal.ONE).shouldBeType<JSONDecimal>()
        JSONValue("hello").shouldBeType<JSONString>()
        JSONValue(true).shouldBeType<JSONBoolean>()
        JSONValue(JSONValue(0), JSONValue(1)).shouldBeType<JSONArray>()
        JSONValue("alpha" to JSONValue(0), "beta" to JSONValue(1)).shouldBeType<JSONObject>()
        JSONValue("alpha" refersTo  JSONValue(0), "beta" refersTo  JSONValue(1)).shouldBeType<JSONObject>()
    }

    companion object {

        @Suppress("RedundantNullableReturnType")
        private fun createJSONValue(): JSONValue? = JSONInt(222)

        private fun createJSONValueNull(): JSONValue? = null

    }

}
