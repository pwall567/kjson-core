/*
 * @(#) ParserCommentTest.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2025 Peter Wall
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
import io.kstuff.test.shouldThrow

import io.kjson.JSON
import io.kjson.JSONArray
import io.kjson.JSONException
import io.kjson.JSONInt
import io.kjson.JSONObject
import io.kjson.parser.ParserErrors.UNCLOSED_COMMENT

class ParserCommentTest {

    @Test fun `should skip slash slash comments`() {
        val json = "{\"a\":123,\"b\":456//\n}"
        shouldThrow<JSONException> { JSON.parse(json) }
        val parseOptions = ParseOptions(slashSlashComment = true)
        with(JSON.parse(json, parseOptions)) {
            shouldBeType<JSONObject>()
            size shouldBe 2
            this["a"] shouldBe JSONInt(123)
            this["b"] shouldBe JSONInt(456)
        }
    }

    @Test fun `should skip multiple slash slash comments`() {
        val json = "[123,// comment\n// another\n456// again\n,789]"
        shouldThrow<JSONException> { JSON.parse(json) }
        val parseOptions = ParseOptions(slashSlashComment = true)
        with(JSON.parse(json, parseOptions)) {
            shouldBeType<JSONArray>()
            size shouldBe 3
            this[0] shouldBe JSONInt(123)
            this[1] shouldBe JSONInt(456)
            this[2] shouldBe JSONInt(789)
        }
    }

    @Test fun `should skip slash star comments`() {
        val json = "{\"a\":123,\"b\":456/**/}"
        shouldThrow<JSONException> { JSON.parse(json) }
        val parseOptions = ParseOptions(slashStarComment = true)
        with(JSON.parse(json, parseOptions)) {
            shouldBeType<JSONObject>()
            size shouldBe 2
            this["a"] shouldBe JSONInt(123)
            this["b"] shouldBe JSONInt(456)
        }
    }

    @Test fun `should skip multiple slash star comments`() {
        val json = "[123,/* comment *//* another */456/* again */,789]"
        shouldThrow<JSONException> { JSON.parse(json) }
        val parseOptions = ParseOptions(slashStarComment = true)
        with(JSON.parse(json, parseOptions)) {
            shouldBeType<JSONArray>()
            size shouldBe 3
            this[0] shouldBe JSONInt(123)
            this[1] shouldBe JSONInt(456)
            this[2] shouldBe JSONInt(789)
        }
    }

    @Test fun `should throw exception on unclosed slash star comment`() {
        val json = "{\"abc\":[333,/*"
        val parseOptions = ParseOptions(slashStarComment = true)
        shouldThrow<ParseException>("$UNCLOSED_COMMENT, at /abc/1") {
            JSON.parse(json, parseOptions)
        }.let {
            it.text shouldBe UNCLOSED_COMMENT
            it.pointer shouldBe "/abc/1"
        }
    }

}
