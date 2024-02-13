/*
 * @(#) JSONObjectTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024 Peter Wall
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
import kotlin.test.assertNull
import kotlin.test.assertTrue
import kotlin.test.expect
import kotlinx.coroutines.runBlocking

import java.math.BigDecimal

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture

class JSONObjectTest {

    @Test fun `should create JSONObject using of`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should create empty JSONObject using of`() {
        val jsonObject = JSONObject.of()
        expect(0) { jsonObject.size }
        expect("{}") { jsonObject.toJSON() }
        expect("{}") { jsonObject.toString() }
        assertTrue(jsonObject.isEmpty())
        assertFalse(jsonObject.isNotEmpty())
    }

    @Test fun `should create JSONObject using from Map`() {
        val map = mapOf<String, JSONValue?>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = JSONObject.from(map)
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should create JSONObject using from List`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = JSONObject.from(list)
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should create JSONObject using fromProperties`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("def", JSONString("X")),
        )
        val jsonObject = JSONObject.fromProperties(properties)
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should build JSONObject using Builder`() {
        val json = JSONObject.Builder {
            add("first", JSONInt(123))
            add("second", JSONString("dummy"))
            assertTrue(containsKey("first"))
            assertTrue(containsKey("second"))
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

    @Test fun `should build JSONObject using build and Property`() {
        val json = JSONObject.build {
            add(JSONObject.Property("first", JSONInt(123)))
            add(JSONObject.Property("second", JSONString("dummy")))
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

    @Test fun `should correctly report containsKey`() {
        assertTrue(simpleObject.containsKey("abc"))
        assertFalse(simpleObject.containsKey("xyz"))
    }

    @Test fun `should correctly report containsValue`() {
        assertTrue(mixedObject.containsValue(JSONInt(123)))
        assertFalse(mixedObject.containsValue(JSONInt(456)))
        assertFalse(mixedObject.containsValue(null))
        assertTrue(mixedObjectWithNull.containsValue(null))
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

    @Test fun `should output JSONObject using appendTo`() {
        val string = buildString {
            simpleObject.appendTo(this)
        }
        expect("""{"abc":12345,"def":"X"}""") { string }
    }

    @Test fun `should format JSONObject using output`() {
        val capture = OutputCapture(64)
        JSONObject.EMPTY.output(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        simpleObject.output(capture)
        expect("""{"abc":12345,"def":"X"}""") { capture.toString() }
    }

    @Test fun `should format JSONObject using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONObject.EMPTY.coOutput(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        simpleObject.coOutput(capture)
        expect("""{"abc":12345,"def":"X"}""") { capture.toString() }
    }

    @Test fun `should get existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        expect(JSONInt(123)) { builder.get("first") }
        expect(JSONString("dummy")) { builder.get("second") }
        assertNull(builder.get("third"))
    }

    @Test fun `should remove existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        expect(JSONInt(123)) { builder.get("first") }
        expect(JSONString("dummy")) { builder.get("second") }
        builder.remove("first")
        assertNull(builder.get("first"))
        val result = builder.build()
        expect(1) { result.size }
        expect(JSONString("dummy")) { result["second"] }
    }

    @Test fun `should fail on incorrect remove of existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        assertFailsWith<JSONException> { builder.remove("third") }.let {
            expect("Key not found - third") { it.message }
        }
    }

    @Test fun `should iterate over object entries`() {
        var count = 0
        mixedObject.forEachEntry { k, v ->
            when (count++) {
                0 -> {
                    expect("first") { k }
                    expect(JSONInt(123)) { v }
                }
                1 -> {
                    expect("second") { k }
                    expect(JSONString("dummy")) { v }
                }
                2 -> {
                    expect("third") { k }
                    expect(JSONLong(123456789123456789)) { v }
                }
                3 -> {
                    expect("fourth") { k }
                    expect(JSONDecimal("0.123")) { v }
                }
                4 -> {
                    expect("fifth") { k }
                    expect(JSONBoolean.TRUE) { v }
                }
            }
        }
        expect(5) { count }
    }

    @Test fun `should iterate over object keys`() {
        var count = 0
        mixedObject.forEachKey {
            when (count++) {
                0 -> expect("first") { it }
                1 -> expect("second") { it }
                2 -> expect("third") { it }
                3 -> expect("fourth") { it }
                4 -> expect("fifth") { it }
            }
        }
        expect(5) { count }
    }

    @Test fun `should iterate over object values`() {
        var count = 0
        mixedObject.forEachValue {
            when (count++) {
                0 -> expect(JSONInt(123)) { it }
                1 -> expect(JSONString("dummy")) { it }
                2 -> expect(JSONLong(123456789123456789)) { it }
                3 -> expect(JSONDecimal("0.123")) { it }
                4 -> expect(JSONBoolean.TRUE) { it }
            }
        }
        expect(5) { count }
    }

    @Test fun `should iterate over object as List`() {
        var count = 0
        for (property in mixedObject) {
            when (count++) {
                0 -> {
                    expect("first") { property.name }
                    expect(JSONInt(123)) { property.value }
                }
                1 -> {
                    expect("second") { property.name }
                    expect(JSONString("dummy")) { property.value }
                }
                2 -> {
                    expect("third") { property.name }
                    expect(JSONLong(123456789123456789)) { property.value }
                }
                3 -> {
                    expect("fourth") { property.name }
                    expect(JSONDecimal("0.123")) { property.value }
                }
                4 -> {
                    expect("fifth") { property.name }
                    expect(JSONBoolean.TRUE) { property.value }
                }
            }
        }
        expect(5) { count }
    }

    @Test fun `should iterate over object using ListIterator`() {
        val iterator = mixedObject.listIterator()
        assertTrue(iterator.hasNext())
        assertFalse(iterator.hasPrevious())
        with(iterator.next()) {
            expect("first") { name }
            expect(JSONInt(123)) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.next()) {
            expect("second") { name }
            expect(JSONString("dummy")) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.next()) {
            expect("third") { name }
            expect(JSONLong(123456789123456789)) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.next()) {
            expect("fourth") { name }
            expect(JSONDecimal("0.123")) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.next()) {
            expect("fifth") { name }
            expect(JSONBoolean.TRUE) { value }
        }
        assertFalse(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("fifth") { name }
            expect(JSONBoolean.TRUE) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("fourth") { name }
            expect(JSONDecimal("0.123")) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("third") { name }
            expect(JSONLong(123456789123456789)) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("second") { name }
            expect(JSONString("dummy")) { value }
        }
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("first") { name }
            expect(JSONInt(123)) { value }
        }
        assertTrue(iterator.hasNext())
        assertFalse(iterator.hasPrevious())
    }

    @Test fun `should iterate over object using ListIterator with start index`() {
        val iterator = mixedObject.listIterator(1)
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        with(iterator.previous()) {
            expect("first") { name }
            expect(JSONInt(123)) { value }
        }
        assertTrue(iterator.hasNext())
        assertFalse(iterator.hasPrevious())
    }

    @Test fun `should not iterate over empty object using ListIterator`() {
        val iterator = JSONObject.EMPTY.listIterator(1)
        assertFalse(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
    }

    @Test fun `should create subset object using subList`() {
        val sub = mixedObject.subList(2, 4)
        expect(2) { sub.size }
        with(sub[0]) {
            expect("third") { name }
            expect(JSONLong(123456789123456789)) { value }
        }
        with(sub[1]) {
            expect("fourth") { name }
            expect(JSONDecimal("0.123")) { value }
        }
    }

    @Test fun `should create Property`() {
        val property = JSONObject.Property("propertyName", JSONInt(12345))
        expect("propertyName") { property.name }
        expect(JSONInt(12345)) { property.value }
        expect("propertyName=12345") { property.toString() }
    }

    @Test fun `should allow destructuring operations on Property`() {
        val property = JSONObject.Property("propertyName", JSONInt(12345))
        val (aaa, bbb) = property
        expect("propertyName") { aaa }
        expect(JSONInt(12345)) { bbb }
    }

    companion object {

        val simpleObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))

        val mixedObject = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", BigDecimal("0.123"))
            add("fifth", true)
        }

        val mixedObjectWithNull = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", BigDecimal("0.123"))
            add("fifth", true)
            add("sixth", null)
        }

    }

}
