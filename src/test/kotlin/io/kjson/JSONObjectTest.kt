/*
 * @(#) JSONObjectTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023, 2024, 2025 Peter Wall
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
import io.kstuff.test.shouldThrow

import io.kjson.testutil.CoOutputCapture
import io.kjson.testutil.OutputCapture
import io.kjson.util.BuilderException

class JSONObjectTest {

    @Test fun `should create JSONObject using of`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using of with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val jsonObject = JSONObject.of(
                "abc" to JSONInt(12345),
                "def" to JSONString("X"),
                duplicateKeyOption = duplicateKeyOption,
            )
            jsonObject.size shouldBe 2
            jsonObject["abc"] shouldBe JSONInt(12345)
            jsonObject["def"] shouldBe JSONString("X")
            jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.isEmpty() shouldBe false
            jsonObject.isNotEmpty() shouldBe true
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using of`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.of("abc" to JSONInt(12345), "abc" to JSONString("X"))
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using of with duplicateKeyOption ERROR`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.of(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR,
            )
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should accept first key creating JSONObject using of with duplicateKeyOption TAKE_FIRST`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept last key creating JSONObject using of with duplicateKeyOption TAKE_LAST`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":"X"}"""
        jsonObject.toString() shouldBe """{"abc":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept matching duplicate creating JSONObject using of with CHECK_IDENTICAL`() {
        val jsonObject = JSONObject.of(
            "abc" to JSONInt(12345),
            "abc" to JSONInt(12345),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should report duplicate key error creating JSONObject using of with CHECK_IDENTICAL`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.of(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should create JSONObject using JSONObject function`() {
        val jsonObject = JSONObject("abc" refersTo JSONInt(12345), "def" refersTo JSONString("X"))
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using JSONObject function with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val jsonObject = JSONObject(
                "abc" refersTo JSONInt(12345),
                "def" refersTo JSONString("X"),
                duplicateKeyOption = duplicateKeyOption
            )
            jsonObject.size shouldBe 2
            jsonObject["abc"] shouldBe JSONInt(12345)
            jsonObject["def"] shouldBe JSONString("X")
            jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.isEmpty() shouldBe false
            jsonObject.isNotEmpty() shouldBe true
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject("abc" to JSONInt(12345), "abc" to JSONString("X"))
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function with option ERROR`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR,
            )
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should accept first key creating JSONObject using JSONObject function with option TAKE_FIRST`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept last key creating JSONObject using JSONObject function with option TAKE_LAST`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONString("X"),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":"X"}"""
        jsonObject.toString() shouldBe """{"abc":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept matching duplicate creating JSONObject using JSONObject function with CHECK_IDENTICAL`() {
        val jsonObject = JSONObject(
            "abc" to JSONInt(12345),
            "abc" to JSONInt(12345),
            duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
        )
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should report duplicate key error creating JSONObject using JSONObject function with CHECK_IDENTICAL`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject(
                "abc" to JSONInt(12345),
                "abc" to JSONString("X"),
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should create JSONObject using from Map`() {
        val map = mapOf<String, JSONValue?>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = JSONObject.from(map)
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using from Map extension function`() {
        val map = mapOf<String, JSONValue?>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = map.toJSONObject()
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using from List`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "def" to JSONString("X"))
        val jsonObject = JSONObject.from(list)
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using from List with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "def" to JSONString("X"))
            val jsonObject = JSONObject.from(list, duplicateKeyOption = duplicateKeyOption)
            jsonObject.size shouldBe 2
            jsonObject["abc"] shouldBe JSONInt(12345)
            jsonObject["def"] shouldBe JSONString("X")
            jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.isEmpty() shouldBe false
            jsonObject.isNotEmpty() shouldBe true
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using from List`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
            JSONObject.from(list)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using from List with duplicateKeyOption ERROR`() {
        shouldThrow<BuilderException>("Duplicate key - abc") {
            val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
            JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should accept first key creating JSONObject using from List with duplicateKeyOption TAKE_FIRST`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept last key creating JSONObject using from List with duplicateKeyOption TAKE_LAST`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":"X"}"""
        jsonObject.toString() shouldBe """{"abc":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept matching duplicate creating JSONObject using from List with CHECK_IDENTICAL`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONInt(12345))
        val jsonObject = JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should report duplicate key error creating JSONObject using from List with CHECK_IDENTICAL`() {
        val list = listOf<Pair<String, JSONValue?>>("abc" to JSONInt(12345), "abc" to JSONString("X"))
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.from(list, duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should create JSONObject using fromProperties`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "def" refersTo JSONString("X"),
        )
        val jsonObject = JSONObject.fromProperties(properties)
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using fromProperties with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val properties = listOf(
                "abc" refersTo JSONInt(12345),
                "def" refersTo JSONString("X"),
            )
            val jsonObject = JSONObject.fromProperties(properties, duplicateKeyOption = duplicateKeyOption)
            jsonObject.size shouldBe 2
            jsonObject["abc"] shouldBe JSONInt(12345)
            jsonObject["def"] shouldBe JSONString("X")
            jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.isEmpty() shouldBe false
            jsonObject.isNotEmpty() shouldBe true
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.fromProperties(properties)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties with option ERROR`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.fromProperties(properties, duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
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
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
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
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":"X"}"""
        jsonObject.toString() shouldBe """{"abc":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
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
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should report duplicate key error creating JSONObject using fromProperties with CHECK_IDENTICAL`() {
        val properties = listOf(
            "abc" refersTo JSONInt(12345),
            "abc" refersTo JSONString("X"),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            JSONObject.fromProperties(
                properties,
                duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL,
            )
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should create JSONObject using extension function`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("def", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject()
        jsonObject.size shouldBe 2
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject["def"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should create JSONObject using extension function with duplicateKeyOption`() {
        for (duplicateKeyOption in JSONObject.DuplicateKeyOption.entries) {
            val properties = listOf(
                JSONObject.Property("abc", JSONInt(12345)),
                JSONObject.Property("def", JSONString("X")),
            )
            val jsonObject = properties.toJSONObject(duplicateKeyOption = duplicateKeyOption)
            jsonObject.size shouldBe 2
            jsonObject["abc"] shouldBe JSONInt(12345)
            jsonObject["def"] shouldBe JSONString("X")
            jsonObject.toJSON() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.toString() shouldBe """{"abc":12345,"def":"X"}"""
            jsonObject.isEmpty() shouldBe false
            jsonObject.isNotEmpty() shouldBe true
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            properties.toJSONObject()
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function with option ERROR`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should accept first key creating JSONObject using extension function with TAKE_FIRST`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept last key creating JSONObject using extension function with  TAKE_LAST`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONString("X")
        jsonObject.toJSON() shouldBe """{"abc":"X"}"""
        jsonObject.toString() shouldBe """{"abc":"X"}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should accept matching duplicate creating JSONObject using extension function with CHECK_IDENTICAL`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONInt(12345)),
        )
        val jsonObject = properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        jsonObject.size shouldBe 1
        jsonObject["abc"] shouldBe JSONInt(12345)
        jsonObject.toJSON() shouldBe """{"abc":12345}"""
        jsonObject.toString() shouldBe """{"abc":12345}"""
        jsonObject.isEmpty() shouldBe false
        jsonObject.isNotEmpty() shouldBe true
    }

    @Test fun `should report duplicate key error creating JSONObject using extension function with CHECK_IDENTICAL`() {
        val properties = listOf(
            JSONObject.Property("abc", JSONInt(12345)),
            JSONObject.Property("abc", JSONString("X")),
        )
        shouldThrow<BuilderException>("Duplicate key - abc") {
            properties.toJSONObject(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "abc"
        }
    }

    @Test fun `should build JSONObject using Builder`() {
        val json = JSONObject.Builder {
            add("first", JSONInt(123))
            add("second", JSONString("dummy"))
            containsKey("first") shouldBe true
            containsKey("second") shouldBe true
        }.build()
        json.size shouldBe 2
        json["first"] shouldBe JSONInt(123)
        json["second"] shouldBe JSONString("dummy")
    }

    @Test fun `should build JSONObject using build`() {
        val json = JSONObject.build {
            add("first", JSONInt(123))
            add("second", JSONString("dummy"))
        }
        json.size shouldBe 2
        json["first"] shouldBe JSONInt(123)
        json["second"] shouldBe JSONString("dummy")
    }

    @Test fun `should build JSONObject using build and Property`() {
        val json = JSONObject.build {
            add(JSONObject.Property("first", JSONInt(123)))
            add(JSONObject.Property("second", JSONString("dummy")))
        }
        json.size shouldBe 2
        json["first"] shouldBe JSONInt(123)
        json["second"] shouldBe JSONString("dummy")
    }

    @Test fun `should build JSONObject using build with non-JSON classes`() {
        val json = JSONObject.build {
            add("first", 123)
            add("second", "dummy")
            add("third", 123456789123456789)
            add("fourth", "0.123".toBigDecimal())
            add("fifth", true)
        }
        json.size shouldBe 5
        json["first"] shouldBe JSONInt(123)
        json["second"] shouldBe JSONString("dummy")
        json["third"] shouldBe JSONLong(123456789123456789)
        json["fourth"] shouldBe JSONDecimal("0.123".toBigDecimal())
        json["fifth"] shouldBe JSONBoolean.TRUE
    }

    @Test fun `should limit Builder to single use`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        builder.containsKey("second") shouldBe true
        shouldThrow<BuilderException>("Duplicate key - second") {
            builder.add("second", JSONString("another"))
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "second"
        }
        builder.size shouldBe 2
        val json = builder.build()
        json.size shouldBe 2
        shouldThrow<BuilderException>("Builder is closed") {
            builder.build()
        }.let {
            it.text shouldBe "Builder is closed"
            it.key shouldBe ""
        }
    }

    @Test fun `should correctly report containsKey`() {
        simpleObject.containsKey("abc") shouldBe true
        simpleObject.containsKey("xyz") shouldBe false
    }

    @Test fun `should correctly report containsValue`() {
        mixedObject.containsValue(JSONInt(123)) shouldBe true
        mixedObject.containsValue(JSONInt(456)) shouldBe false
        mixedObject.containsValue(null) shouldBe false
        mixedObjectWithNull.containsValue(null) shouldBe true
    }

    @Test fun `should compare to other Map`() {
        val json = JSONObject.Builder {
            add("alpha", JSONInt(1111))
            add("beta", JSONString("hello"))
        }.build()
        val map = mapOf("alpha" to JSONInt(1111), "beta" to JSONString("hello"))
        map.shouldBe<Map<*, *>>(json)
        json.shouldBe<Map<*, *>>(map)
        map.hashCode() shouldBe json.hashCode()
    }

    @Test fun `should allow use of keys`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        jsonObject.size shouldBe 2
        val keysIterator = jsonObject.keys.iterator()
        keysIterator.next() shouldBe "abc"
        keysIterator.next() shouldBe "def"
        keysIterator.hasNext() shouldBe false
    }

    @Test fun `should allow use of values`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        jsonObject.size shouldBe 2
        val valuesIterator = jsonObject.values.iterator()
        valuesIterator.next() shouldBe JSONInt(12345)
        valuesIterator.next() shouldBe JSONString("X")
        valuesIterator.hasNext() shouldBe false
    }

    @Test fun `should allow use of entries`() {
        val jsonObject = JSONObject.of("abc" to JSONInt(12345), "def" to JSONString("X"))
        jsonObject.size shouldBe 2
        val entriesIterator = jsonObject.entries.iterator()
        val entry1 = entriesIterator.next()
        entry1.key shouldBe "abc"
        entry1.value shouldBe JSONInt(12345)
        val entry2 = entriesIterator.next()
        entry2.key shouldBe "def"
        entry2.value shouldBe JSONString("X")
        entriesIterator.hasNext() shouldBe false
        for ((a, b) in jsonObject.entries) { // check that destructuring works
            if (a == "abc")
                b shouldBe JSONInt(12345)
            if (a == "def")
                b shouldBe JSONString("X")
        }
    }

    @Test fun `should output JSONObject using appendTo`() {
        val string = buildString {
            simpleObject.appendTo(this)
        }
        string shouldBe """{"abc":12345,"def":"X"}"""
    }

    @Test fun `should format JSONObject using output`() {
        val capture = OutputCapture(64)
        JSONObject.EMPTY.outputTo(capture)
        capture.toString() shouldBe "{}"
        capture.reset()
        simpleObject.outputTo(capture)
        capture.toString() shouldBe """{"abc":12345,"def":"X"}"""
    }

    @Test fun `should format JSONObject using coOutput`() = runBlocking {
        val capture = CoOutputCapture(64)
        JSONObject.EMPTY.coOutputTo(capture)
        capture.toString() shouldBe "{}"
        capture.reset()
        simpleObject.coOutputTo(capture)
        capture.toString() shouldBe """{"abc":12345,"def":"X"}"""
    }

    @Test fun `should get existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        builder.get("first") shouldBe JSONInt(123)
        builder.get("second") shouldBe JSONString("dummy")
        builder.get("third") shouldBe null
    }

    @Test fun `should remove existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        builder.get("first") shouldBe JSONInt(123)
        builder.get("second") shouldBe JSONString("dummy")
        builder.remove("first")
        builder.get("first") shouldBe null
        val result = builder.build()
        result.size shouldBe 1
        result["second"] shouldBe JSONString("dummy")
    }

    @Test fun `should fail on incorrect remove of existing Builder entries`() {
        val builder = JSONObject.Builder()
        builder.add("first", JSONInt(123))
        builder.add("second", JSONString("dummy"))
        shouldThrow<BuilderException>("Key not found - third") {
            builder.remove("third")
        }.let {
            it.text shouldBe "Key not found"
            it.key shouldBe "third"
        }
    }

    @Test fun `should iterate over object entries`() {
        var count = 0
        mixedObject.forEachEntry { k, v ->
            when (count++) {
                0 -> {
                    k shouldBe "first"
                    v shouldBe JSONInt(123)
                }
                1 -> {
                    k shouldBe "second"
                    v shouldBe JSONString("dummy")
                }
                2 -> {
                    k shouldBe "third"
                    v shouldBe JSONLong(123456789123456789)
                }
                3 -> {
                    k shouldBe "fourth"
                    v shouldBe JSONDecimal("0.123")
                }
                4 -> {
                    k shouldBe "fifth"
                    v shouldBe JSONBoolean.TRUE
                }
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over object properties`() {
        var count = 0
        mixedObject.forEachProperty {
            when (count++) {
                0 -> {
                    it.name shouldBe "first"
                    it.value shouldBe JSONInt(123)
                }
                1 -> {
                    it.name shouldBe "second"
                    it.value shouldBe JSONString("dummy")
                }
                2 -> {
                    it.name shouldBe "third"
                    it.value shouldBe JSONLong(123456789123456789)
                }
                3 -> {
                    it.name shouldBe "fourth"
                    it.value shouldBe JSONDecimal("0.123")
                }
                4 -> {
                    it.name shouldBe "fifth"
                    it.value shouldBe JSONBoolean.TRUE
                }
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over object keys`() {
        var count = 0
        mixedObject.forEachKey {
            when (count++) {
                0 -> it shouldBe "first"
                1 -> it shouldBe "second"
                2 -> it shouldBe "third"
                3 -> it shouldBe "fourth"
                4 -> it shouldBe "fifth"
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over object values`() {
        var count = 0
        mixedObject.forEachValue {
            when (count++) {
                0 -> it shouldBe JSONInt(123)
                1 -> it shouldBe JSONString("dummy")
                2 -> it shouldBe JSONLong(123456789123456789)
                3 -> it shouldBe JSONDecimal("0.123")
                4 -> it shouldBe JSONBoolean.TRUE
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over object as List`() {
        var count = 0
        for (property in mixedObject) {
            when (count++) {
                0 -> {
                    property.name shouldBe "first"
                    property.value shouldBe JSONInt(123)
                }
                1 -> {
                    property.name shouldBe "second"
                    property.value shouldBe JSONString("dummy")
                }
                2 -> {
                    property.name shouldBe "third"
                    property.value shouldBe JSONLong(123456789123456789)
                }
                3 -> {
                    property.name shouldBe "fourth"
                    property.value shouldBe JSONDecimal("0.123")
                }
                4 -> {
                    property.name shouldBe "fifth"
                    property.value shouldBe JSONBoolean.TRUE
                }
            }
        }
        count shouldBe 5
    }

    @Test fun `should iterate over object using ListIterator`() {
        val iterator = mixedObject.listIterator()
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe false
        with(iterator.next()) {
            name shouldBe "first"
            value shouldBe JSONInt(123)
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.next()) {
            name shouldBe "second"
            value shouldBe JSONString("dummy")
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.next()) {
            name shouldBe "third"
            value shouldBe JSONLong(123456789123456789)
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.next()) {
            name shouldBe "fourth"
            value shouldBe JSONDecimal("0.123")
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.next()) {
            name shouldBe "fifth"
            value shouldBe JSONBoolean.TRUE
        }
        iterator.hasNext() shouldBe false
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "fifth"
            value shouldBe JSONBoolean.TRUE
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "fourth"
            value shouldBe JSONDecimal("0.123")
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "third"
            value shouldBe JSONLong(123456789123456789)
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "second"
            value shouldBe JSONString("dummy")
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "first"
            value shouldBe JSONInt(123)
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe false
    }

    @Test fun `should iterate over object using ListIterator with start index`() {
        val iterator = mixedObject.listIterator(1)
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe true
        with(iterator.previous()) {
            name shouldBe "first"
            value shouldBe JSONInt(123)
        }
        iterator.hasNext() shouldBe true
        iterator.hasPrevious() shouldBe false
    }

    @Test fun `should not iterate over empty object using ListIterator`() {
        val iterator = JSONObject.EMPTY.listIterator(1)
        iterator.hasNext() shouldBe false
        iterator.hasPrevious() shouldBe true
    }

    @Test fun `should create subset object using subList`() {
        val sub = mixedObject.subList(2, 4)
        sub.size shouldBe 2
        sub[0] shouldBe JSONObject.Property("third", JSONLong(123456789123456789))
        sub[1] shouldBe JSONObject.Property("fourth", JSONDecimal("0.123"))
    }

    @Test fun `should create Property`() {
        val property = JSONObject.Property("propertyName", JSONInt(12345))
        property.name shouldBe "propertyName"
        property.value shouldBe JSONInt(12345)
        property.toString() shouldBe "propertyName=12345"
    }

    @Test fun `should allow destructuring operations on Property`() {
        val property = JSONObject.Property("propertyName", JSONInt(12345))
        val (aaa, bbb) = property
        aaa shouldBe "propertyName"
        bbb shouldBe JSONInt(12345)
    }

    @Test fun `should respect DuplicateKeyOption ERROR`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR)
        builder.add("first", 111)
        builder.add("second", 222)
        shouldThrow<BuilderException>("Duplicate key - second") {
            builder.add("second", 333)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "second"
        }
    }

    @Test fun `should respect DuplicateKeyOption TAKE_FIRST`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_FIRST)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 333)
        with(builder.build()) {
            size shouldBe 2
            this["first"] shouldBe JSONInt(111)
            this["second"] shouldBe JSONInt(222)
        }
    }

    @Test fun `should respect DuplicateKeyOption TAKE_LAST`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.TAKE_LAST)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 333)
        with(builder.build()) {
            size shouldBe 2
            this["first"] shouldBe JSONInt(111)
            this["second"] shouldBe JSONInt(333)
        }
    }

    @Test fun `should respect DuplicateKeyOption CHECK_IDENTICAL`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        builder.add("first", 111)
        builder.add("second", 222)
        builder.add("second", 222)
        with(builder.build()) {
            size shouldBe 2
            this["first"] shouldBe JSONInt(111)
            this["second"] shouldBe JSONInt(222)
        }
    }

    @Test fun `should fail on DuplicateKeyOption CHECK_IDENTICAL with different values`() {
        val builder = JSONObject.Builder(duplicateKeyOption = JSONObject.DuplicateKeyOption.CHECK_IDENTICAL)
        builder.add("first", 111)
        builder.add("second", 222)
        shouldThrow<BuilderException>("Duplicate key - second") {
            builder.add("second", 333)
        }.let {
            it.text shouldBe "Duplicate key"
            it.key shouldBe "second"
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
