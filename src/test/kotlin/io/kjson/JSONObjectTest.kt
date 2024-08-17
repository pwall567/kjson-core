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

    @Test fun `should create JSONObject using of with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val jsonObject = JSONObject.of(
                "abc" to JSONInt(12345),
                "def" to JSONString("X"),
                duplicateKeyOption = duplicateKeyOption,
            )
            expect(2) { jsonObject.size }
            expect(JSONInt(12345)) { jsonObject["abc"] }
            expect(JSONString("X")) { jsonObject["def"] }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
            assertFalse(jsonObject.isEmpty())
            assertTrue(jsonObject.isNotEmpty())
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using of`() {
        assertFailsWith<JSONException> {
            JSONObject.of("abc" to JSONInt(12345), "abc" to JSONString("X"))
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using of with duplicateKeyOption ERROR`() {
        assertFailsWith<JSONException> {
            JSONObject.of(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR,
            )
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should accept first key creating JSONObject using of with duplicateKeyOption TAKE_FIRST`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept last key creating JSONObject using of with duplicateKeyOption TAKE_LAST`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST,
        )
        expect(1) { jsonObject.size }
        expect(JSONString("X")) { jsonObject["abc"] }
        expect("""{"abc":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept matching duplicate creating JSONObject using of with CHECK_IDENTICAL`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONInt(12345),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should report duplicate key error creating JSONObject using of with CHECK_IDENTICAL`() {
        assertFailsWith<JSONException> {
            JSONObject.of(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should create JSONObject using JSONObject function`() {
        val jsonObject = JSONObject("abc" refersTo JSONInt(12345), "def" refersTo JSONString("X"))
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should create JSONObject using JSONObject function with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val jsonObject = JSONObject(
                "abc" refersTo JSONInt(12345),
                "def" refersTo JSONString("X"),
                duplicateKeyOption = duplicateKeyOption
            )
            expect(2) { jsonObject.size }
            expect(JSONInt(12345)) { jsonObject["abc"] }
            expect(JSONString("X")) { jsonObject["def"] }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
            assertFalse(jsonObject.isEmpty())
            assertTrue(jsonObject.isNotEmpty())
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function`() {
        assertFailsWith<JSONException> {
            JSONObject("abc" to JSONInt(12345), "abc" to JSONString("X"))
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function with option ERROR`() {
        assertFailsWith<JSONException> {
            JSONObject(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR,
            )
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should accept first key creating JSONObject using JSONObject function with option TAKE_FIRST`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept last key creating JSONObject using JSONObject function with option TAKE_LAST`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST,
        )
        expect(1) { jsonObject.size }
        expect(JSONString("X")) { jsonObject["abc"] }
        expect("""{"abc":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept matching duplicate creating JSONObject using JSONObject function with CHECK_IDENTICAL`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONInt(12345),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function with CHECK_IDENTICAL`() {
        assertFailsWith<JSONException> {
            JSONObject(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
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

    @Test fun `should create JSONObject using from Map extension function`() {
        val map = mapOf<String, JSONValue?>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = map.toJSONObject()
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

    @Test fun `should create JSONObject using from List with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "def" to JSONString("X"))
            val jsonObject = JSONObject.from(list, duplicateKeyOption = duplicateKeyOption)
            expect(2) { jsonObject.size }
            expect(JSONInt(12345)) { jsonObject["abc"] }
            expect(JSONString("X")) { jsonObject["def"] }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
            assertFalse(jsonObject.isEmpty())
            assertTrue(jsonObject.isNotEmpty())
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using from List`() {
        assertFailsWith<JSONException> {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
            JSONObject.from(list)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using from List with duplicateKeyOption ERROR`() {
        assertFailsWith<JSONException> {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
            JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should accept first key creating JSONObject using from List with duplicateKeyOption TAKE_FIRST`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept last key creating JSONObject using from List with duplicateKeyOption TAKE_LAST`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        expect(1) { jsonObject.size }
        expect(JSONString("X")) { jsonObject["abc"] }
        expect("""{"abc":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept matching duplicate creating JSONObject using from List with CHECK_IDENTICAL`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONInt(12345))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should report duplicate key error creating JSONObject using from List with CHECK_IDENTICAL`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        assertFailsWith<JSONException> {
            JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should create JSONObject using fromProperties`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "def" refersTo JSONString("X"),
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

    @Test fun `should create JSONObject using fromProperties with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val properties = listOf(
                "abc" refersTo JSONInt(12345),
                "def" refersTo JSONString("X"),
            )
            val jsonObject = JSONObject.fromProperties(properties, duplicateKeyOption = duplicateKeyOption)
            expect(2) { jsonObject.size }
            expect(JSONInt(12345)) { jsonObject["abc"] }
            expect(JSONString("X")) { jsonObject["def"] }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
            assertFalse(jsonObject.isEmpty())
            assertTrue(jsonObject.isNotEmpty())
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        assertFailsWith<JSONException> {
            JSONObject.fromProperties(properties)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties with option ERROR`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        assertFailsWith<JSONException> {
            JSONObject.fromProperties(properties, duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should accept first key creating JSONObject using fromProperties with duplicateKeyOption TAKE_FIRST`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        val jsonObject = JSONObject.fromProperties(
            properties,
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept last key creating JSONObject using fromProperties with duplicateKeyOption TAKE_LAST`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        val jsonObject = JSONObject.fromProperties(
            properties,
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST,
        )
        expect(1) { jsonObject.size }
        expect(JSONString("X")) { jsonObject["abc"] }
        expect("""{"abc":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept matching duplicate creating JSONObject using fromProperties with CHECK_IDENTICAL`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONInt(12345),
        )
        val jsonObject = JSONObject.fromProperties(
            properties,
            duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
        )
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties with CHECK_IDENTICAL`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        assertFailsWith<JSONException> {
            JSONObject.fromProperties(
                properties,
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should create JSONObject using extension function`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("def", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject()
        expect(2) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect(JSONString("X")) { jsonObject["def"] }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should create JSONObject using extension function with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val properties = listOf(
                JSONObject.Property("abc", JSONInt(12345)),
                JSONObject.Property("def", JSONString("X")),
            )
            val jsonObject = properties.toJSONObject(duplicateKeyOption = duplicateKeyOption)
            expect(2) { jsonObject.size }
            expect(JSONInt(12345)) { jsonObject["abc"] }
            expect(JSONString("X")) { jsonObject["def"] }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toJSON() }
            expect("""{"abc":12345,"def":"X"}""") { jsonObject.toString() }
            assertFalse(jsonObject.isEmpty())
            assertTrue(jsonObject.isNotEmpty())
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        assertFailsWith<JSONException> {
            properties.toJSONObject()
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function with option ERROR`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        assertFailsWith<JSONException> {
            properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
    }

    @Test fun `should accept first key creating JSONObject using extension function with TAKE_FIRST`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept last key creating JSONObject using extension function with  TAKE_LAST`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        expect(1) { jsonObject.size }
        expect(JSONString("X")) { jsonObject["abc"] }
        expect("""{"abc":"X"}""") { jsonObject.toJSON() }
        expect("""{"abc":"X"}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should accept matching duplicate creating JSONObject using extension function with CHECK_IDENTICAL`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONInt(12345)),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        expect(1) { jsonObject.size }
        expect(JSONInt(12345)) { jsonObject["abc"] }
        expect("""{"abc":12345}""") { jsonObject.toJSON() }
        expect("""{"abc":12345}""") { jsonObject.toString() }
        assertFalse(jsonObject.isEmpty())
        assertTrue(jsonObject.isNotEmpty())
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function with CHECK_IDENTICAL`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        assertFailsWith<JSONException> {
            properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        }.let {
            expect("Duplicate key - abc") { it.message }
        }
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
            add("fourth", "0.123".toBigDecimal())
            add("fifth", true)
        }
        expect(5) { json.size }
        expect(JSONInt(123)) { json["first"] }
        expect(JSONString("dummy")) { json["second"] }
        expect(JSONLong(123456789123456789)) { json["third"] }
        expect(JSONDecimal("0.123".toBigDecimal())) { json["fourth"] }
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
        val map = mapOf("alpha" to JSONInt(1111), "beta" to JSONString("hello"))
        assertEquals<Map<*, *>>(json, map)
        assertEquals<Map<*, *>>(map, json)
        assertEquals(json.hashCode(), map.hashCode())
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
        JSONObject.EMPTY.outputTo(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        simpleObject.outputTo(capture)
        expect("""{"abc":12345,"def":"X"}""") { capture.toString() }
    }

    @Test fun `should format JSONObject using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONObject.EMPTY.coOutputTo(capture)
        expect("{}") { capture.toString() }
        capture.reset()
        simpleObject.coOutputTo(capture)
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

    @Test fun `should iterate over object properties`() {
        var count = 0
        mixedObject.forEachProperty {
            when (count++) {
                0 -> {
                    expect("first") { it.name }
                    expect(JSONInt(123)) { it.value }
                }
                1 -> {
                    expect("second") { it.name }
                    expect(JSONString("dummy")) { it.value }
                }
                2 -> {
                    expect("third") { it.name }
                    expect(JSONLong(123456789123456789)) { it.value }
                }
                3 -> {
                    expect("fourth") { it.name }
                    expect(JSONDecimal("0.123")) { it.value }
                }
                4 -> {
                    expect("fifth") { it.name }
                    expect(JSONBoolean.TRUE) { it.value }
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
        expect(JSONObject.Property("third", JSONLong(123456789123456789))) { sub[0] }
        expect(JSONObject.Property("fourth", JSONDecimal("0.123"))) { sub[1] }
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

    @Test fun `should respect DuplicateKeyOption ERROR`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        builder.add("first", 111)
        builder.add("second", 222)
        assertFailsWith<JSONException> { builder.add("second", 333) }.let {
            expect("Duplicate key - second") { it.message }
        }
    }

    @Test fun `should respect DuplicateKeyOption TAKE_FIRST`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 333)
        with(builder.build()) {
            expect(2) { size }
            expect(JSONInt(111)) { this["first"] }
            expect(JSONInt(222)) { this["second"] }
        }
    }

    @Test fun `should respect DuplicateKeyOption TAKE_LAST`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 333)
        with(builder.build()) {
            expect(2) { size }
            expect(JSONInt(111)) { this["first"] }
            expect(JSONInt(333)) { this["second"] }
        }
    }

    @Test fun `should respect DuplicateKeyOption CHECK_IDENTICAL`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 222)
        with(builder.build()) {
            expect(2) { size }
            expect(JSONInt(111)) { this["first"] }
            expect(JSONInt(222)) { this["second"] }
        }
    }

    @Test fun `should fail on DuplicateKeyOption CHECK_IDENTICAL with different values`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        builder.add("first", 111)
        builder.add("second", 222)
        assertFailsWith<JSONException> { builder.add("second", 333) }.let {
            expect("Duplicate key - second") { it.message }
        }
    }

    companion object {

        val simpleObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))

        val mixedObject = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", "0.123".toBigDecimal())
            add("fifth", true)
        }

        val mixedObjectWithNull = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", "0.123".toBigDecimal())
            add("fifth", true)
            add("sixth", null)
        }

    }

}
