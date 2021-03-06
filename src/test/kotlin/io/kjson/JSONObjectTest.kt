/*
 * @(#) JSONObjectTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022 Peter Wall
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
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import java.math.BigDecimal
import io.kjson.util.CoOutputCapture
import io.kjson.util.OutputCapture

class JSONObjectTest {

    @Test fun `should create JSONObject using of`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
    }

    @Test fun `should build JSONObject using Builder`() {
        val json = JSONObject.Builder {
            add("first", JSONInt(123))
            add("second", JSONString("dummy"))
        }.build()
        expect(2) { json.size }
        expect(JSONInt(123)) { json["first"] }
        expect(JSONString("dummy")) { json["second"] }
    }

    @Test fun `should build JSONObject using build`() {
        val json = JSONObject.build {
            add("first", JSONInt(123))
            add("second", JSONString("dummy"))
        }
        expect(2) { json.size }
        expect(JSONInt(123)) { json["first"] }
        expect(JSONString("dummy")) { json["second"] }
    }

    @Test fun `should build JSONObject using build with non-JSON classes`() {
        val json = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", BigDecimal("0.123"))
            add("fifth", true)
        }
        expect(5) { json.size }
        expect(JSONInt(123)) { json["first"] }
        expect(JSONString("dummy")) { json["second"] }
        expect(JSONLong(123456789123456789)) { json["third"] }
        expect(JSONDecimal(BigDecimal("0.123"))) { json["fourth"] }
        expect(JSONBoolean.TRUE) { json["fifth"] }
    }

    @Test fun `should limit Builder to single use`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        assertTrue(builder.containsKey("second"))
        assertFailsWith<JSONException> { builder.add("second", JSONString("another")) }.let {
            expect("Duplicate key - second") { it.message }
        }
        expect(2) { builder.size }
        val json = builder.build()
        expect(2) { json.size }
        assertFailsWith<JSONException> { builder.build() }.let {
            expect("Builder is closed") { it.message }
        }
    }

    @Test fun `should compare to other Map`() {
        val json = JSONObject.Builder {
            add("alpha", JSONInt(1111))
            add("beta", JSONString("hello"))
        }.build()
        assertEquals<Map<*, *>>(json, mapOf("alpha" to JSONInt(1111), "beta" to JSONString("hello")))
        assertEquals<Map<*, *>>(mapOf("alpha" to JSONInt(1111), "beta" to JSONString("hello")), json)
    }

    @Test fun `should allow use of keys`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        expect(2) { jsonObject.size }
        val keysIterator = jsonObject.keys.iterator()
        expect("abc") { keysIterator.next() }
        expect("def") { keysIterator.next() }
        assertFalse { keysIterator.hasNext() }
    }

    @Test fun `should allow use of values`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        expect(2) { jsonObject.size }
        val valuesIterator = jsonObject.values.iterator()
        expect(JSONInt(12345)) { valuesIterator.next() }
        expect(JSONString("X")) { valuesIterator.next() }
        assertFalse { valuesIterator.hasNext() }
    }

    @Test fun `should allow use of entries`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        expect(2) { jsonObject.size }
        val entriesIterator = jsonObject.entries.iterator()
        val entry1 = entriesIterator.next()
        expect("abc") { entry1.key }
        expect(JSONInt(12345)) { entry1.value }
        val entry2 = entriesIterator.next()
        expect("def") { entry2.key }
        expect(JSONString("X")) { entry2.value }
        assertFalse { entriesIterator.hasNext() }
        for ((a, b) in jsonObject.entries) { // check that destructuring works
            if (a == "abc")
                expect(JSONInt(12345)) { b }
            if (a == "def")
                expect(JSONString("X")) { b }
        }
    }

    @Test fun `should format JSONObject using output`() {
        val capture = OutputCapture(64)
        JSONObject.EMPTY.output(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X")).output(capture)
        expect("""{"abc":12345,"def":"X"}""") { capture.toString() }
    }

    @Test fun `should format JSONObject using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONObject.EMPTY.coOutput(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X")).coOutput(capture)
        expect("""{"abc":12345,"def":"X"}""") { capture.toString() }
    }

}
