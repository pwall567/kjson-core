/*
 * @(#) JSONStringTest.kt
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
import kotlin.test.assertSame
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import io.kjson.JSON.asString
import io.kjson.util.CoOutputCapture
import io.kjson.util.OutputCapture

class JSONStringTest {

    @Test fun `should create JSONString`() {
        val testString = JSONString("ab\u2014c\n")
        expect("ab\u2014c\n") { testString.value }
        expect("\"ab\\u2014c\\n\"") { testString.toJSON() }
        expect("ab\u2014c\n") { testString.toString() }
    }

    @Test fun `should create JSONString using of`() {
        val testString = JSONString.of("Hello!")
        expect("Hello!") { testString.value }
        expect("\"Hello!\"") { testString.toJSON() }
        expect("Hello!") { testString.toString() }
    }

    @Test fun `should use EMPTY`() {
        val testString = JSONString.of("")
        assertSame(JSONString.EMPTY, testString)
        expect("") { testString.toString() }
    }

    @Test fun `should get value using stringValue`() {
        val json = JSON.parse("\"abc\"")
        expect("abc") { json.asString }
    }

    @Test fun `should return JSONString from subSequence`() {
        val json = JSONString.of("irrational")
        val substring = json.subSequence(2, 7)
        expect("\"ratio\"") { substring.toJSON() }
    }

    @Test fun `should format JSONString using output`() {
        val capture = OutputCapture(16)
        JSONString.EMPTY.output(capture)
        expect("\"\"") { capture.toString() }
        capture.reset()
        JSONString("Kia ora").output(capture)
        expect("\"Kia ora\"") { capture.toString() }
    }

    @Test fun `should format JSONString using coOutput`() = runBlocking {
        val capture = CoOutputCapture(16)
        JSONString.EMPTY.coOutput(capture)
        expect("\"\"") { capture.toString() }
        capture.reset()
        JSONString("Kia ora").coOutput(capture)
        expect("\"Kia ora\"") { capture.toString() }
    }

}
