/*
 * @(#) JSONBooleanTest.kt
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
import kotlin.test.assertTrue
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import io.kjson.util.CoOutputCapture
import io.kjson.util.OutputCapture

class JSONBooleanTest {

    @Test fun `should return a JSONBoolean`() {
        val test1 = JSONBoolean.of(true)
        assertSame(JSONBoolean.TRUE, test1)
        expect(true) { test1.value }
        expect("true") { test1.toJSON() }
        expect("true") { test1.toString() }
        val test2 = JSONBoolean.of(false)
        assertSame(JSONBoolean.FALSE, test2)
        expect(false) { test2.value }
        expect("false") { test2.toJSON() }
        expect("false") { test2.toString() }
    }

    @Test fun `should handle JSONBoolean in an array`() {
        val test1 = JSON.parse("[12,true,false]")
        assertTrue(test1 is JSONArray)
        expect(3) { test1.size }
        expect(JSONInt(12)) { test1[0] }
        expect(JSONBoolean.TRUE) { test1[1] }
        expect(JSONBoolean.FALSE) { test1[2] }
    }

    @Test fun `should handle JSONBoolean in an object`() {
        val test1 = JSON.parse("""{"a":true,"b":false}""")
        assertTrue(test1 is JSONObject)
        expect(2) { test1.size }
        expect(JSONBoolean.TRUE) { test1["a"] }
        expect(JSONBoolean.FALSE) { test1["b"] }
    }

    @Test fun `should format JSONBoolean using output`() {
        val capture = OutputCapture(8)
        JSONBoolean.TRUE.output(capture)
        expect("true") { capture.toString() }
        capture.reset()
        JSONBoolean.FALSE.output(capture)
        expect("false") { capture.toString() }
    }

    @Test fun `should format JSONBoolean using coOutput`() = runBlocking {
        val capture = CoOutputCapture(8)
        JSONBoolean.TRUE.coOutput(capture)
        expect("true") { capture.toString() }
        capture.reset()
        JSONBoolean.FALSE.coOutput(capture)
        expect("false") { capture.toString() }
    }

}
