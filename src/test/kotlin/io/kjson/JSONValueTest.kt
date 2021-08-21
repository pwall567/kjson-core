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
import kotlin.test.expect

import io.kjson.JSON.appendJSONValue
import io.kjson.JSON.appendTo
import io.kjson.JSON.toJSON

class JSONValueTest {

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
        test.appendJSONValue(JSONArray.of(JSONInt(123), JSONInt(321)))
        expect("[123,321]") { test.toString() }
    }

    @Suppress("RedundantNullableReturnType")
    private fun createJSONValue(): JSONValue? {
        return JSONInt(222)
    }

}
