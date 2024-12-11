/*
 * @(#) ParserKeywordTest.kt
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

package io.kjson.parser

import kotlin.test.Test

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeType

import io.kjson.JSONBoolean
import io.kjson.JSONObject

class ParserKeywordTest {

    @Test fun `should parse null`() {
        Parser.parse("null") shouldBe null
    }

    @Test fun `should parse true`() {
        val result = Parser.parse("true")
        result.shouldBeType<JSONBoolean>()
        result.value shouldBe true
    }

    @Test fun `should parse false`() {
        val result = Parser.parse("false")
        result.shouldBeType<JSONBoolean>()
        result.value shouldBe false
    }

    @Test fun `should parse keywords in object`() {
        val result = Parser.parse("""{"aaa":true,"bbb":false,"ccc":null}""")
        result.shouldBeType<JSONObject>()
        result.size shouldBe 3
        val aaa = result["aaa"]
        aaa.shouldBeType<JSONBoolean>()
        aaa.value shouldBe true
        val bbb = result["bbb"]
        bbb.shouldBeType<JSONBoolean>()
        bbb.value shouldBe false
        result["ccc"] shouldBe null
    }

}
