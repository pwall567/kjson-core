/*
 * @(#) JSONStringTest.kt
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

import io.kjson.JSON.asString
import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONStringTest {

    @Test fun `should create JSONString`() {
        val testString = JSONString("ab\u2014c\n")
        testString.value shouldBe "ab\u2014c\n"
        testString.toJSON() shouldBe "\"ab\\u2014c\\n\""
        testString.toString() shouldBe "ab\u2014c\n"
    }

    @Test fun `should create JSONString using of`() {
        val testString = JSONString.of("Hello!")
        testString.value shouldBe "Hello!"
        testString.toJSON() shouldBe "\"Hello!\""
        testString.toString() shouldBe "Hello!"
    }

    @Test fun `should use EMPTY`() {
        val testString = JSONString.of("")
        testString shouldBe JSONString.EMPTY
        testString.toString() shouldBe JSONString.EMPTY_STRING
        testString.toJSON() shouldBe "\"\""
    }

    @Test fun `should get value using stringValue`() {
        val json = JSON.parse("\"abc\"")
        json.asString shouldBe "abc"
    }

    @Test fun `should return JSONString from subSequence`() {
        val json = JSONString.of("irrational")
        val substring = json.subSequence(2, 7)
        substring.toJSON() shouldBe "\"ratio\""
    }

    @Test fun `should format JSONString using output`() {
        val capture = OutputCapture(16)
        JSONString.EMPTY.outputTo(capture)
        capture.toString() shouldBe "\"\""
        capture.reset()
        JSONString("Kia ora").outputTo(capture)
        capture.toString() shouldBe "\"Kia ora\""
    }

    @Test fun `should format JSONString using coOutput`() = runBlocking {
        val capture = CoOutputCapture(16)
        JSONString.EMPTY.coOutputTo(capture)
        capture.toString() shouldBe "\"\""
        capture.reset()
        JSONString("Kia ora").coOutputTo(capture)
        capture.toString() shouldBe "\"Kia ora\""
    }

    @Test fun `should build a JSONString using build function`() {
        val json = JSONString.build {
            append('C')
            append("able")
            append(99)
        }
        json shouldBe JSONString("Cable99")
    }

    @Test fun `should return EMPTY when using build function with no content`() {
        val json = JSONString.build {}
        json shouldBe JSONString.EMPTY
    }

}
