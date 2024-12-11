/*
 * @(#) JSONBooleanTest.kt
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

import kotlinx.coroutines.runBlocking

import io.kstuff.test.shouldBe
import io.kstuff.test.shouldBeSameInstance
import io.kstuff.test.shouldBeType

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONBooleanTest {

    @Test fun `should return a JSONBoolean`() {
        val test1 = JSONBoolean.of(true)
        test1 shouldBeSameInstance JSONBoolean.TRUE
        test1.value shouldBe true
        test1.toJSON() shouldBe "true"
        test1.toString() shouldBe "true"
        val test2 = JSONBoolean.of(false)
        test2 shouldBeSameInstance JSONBoolean.FALSE
        test2.value shouldBe false
        test2.toJSON() shouldBe "false"
        test2.toString() shouldBe "false"
    }

    @Test fun `should handle JSONBoolean in an array`() {
        val test1 = JSON.parse("[12,true,false]")
        test1.shouldBeType<JSONArray>()
        test1.size shouldBe 3
        test1[0] shouldBe JSONInt(12)
        test1[1] shouldBe JSONBoolean.TRUE
        test1[2] shouldBe JSONBoolean.FALSE
    }

    @Test fun `should handle JSONBoolean in an object`() {
        val test1 = JSON.parse("""{"a":true,"b":false}""")
        test1.shouldBeType<JSONObject>()
        test1.size shouldBe 2
        test1["a"] shouldBe JSONBoolean.TRUE
        test1["b"] shouldBe JSONBoolean.FALSE
    }

    @Test fun `should format JSONBoolean using output`() {
        val capture = OutputCapture(8)
        JSONBoolean.TRUE.outputTo(capture)
        capture.toString() shouldBe "true"
        capture.reset()
        JSONBoolean.FALSE.outputTo(capture)
        capture.toString() shouldBe "false"
    }

    @Test fun `should format JSONBoolean using coOutput`() = runBlocking {
        val capture = CoOutputCapture(8)
        JSONBoolean.TRUE.coOutputTo(capture)
        capture.toString() shouldBe "true"
        capture.reset()
        JSONBoolean.FALSE.coOutputTo(capture)
        capture.toString() shouldBe "false"
    }

}
