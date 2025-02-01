/*
 * @(#) ParseOptions.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2022, 2023, 2025 Peter Wall
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

import io.jstuff.json.JSONFunctions
import io.jstuff.text.TextMatcher
import io.kjson.JSONObject
import io.kjson.parser.ParserErrors.MAX_DEPTH_ERROR
import io.kjson.parser.ParserErrors.UNCLOSED_COMMENT

/**
 * Options to control parsing.
 *
 * @author  Peter Wall
 */
data class ParseOptions(
    val objectKeyDuplicate: JSONObject.DuplicateKeyOption = JSONObject.DuplicateKeyOption.ERROR,
    val objectKeyUnquoted: Boolean = false,
    val objectTrailingComma: Boolean = false,
    val arrayTrailingComma: Boolean = false,
    val maximumNestingDepth: Int = 1000,
    val slashSlashComment: Boolean = false,
    val slashStarComment: Boolean = false,
) {

    init {
        require(maximumNestingDepth in 1..1200) { "$MAX_DEPTH_ERROR, was $maximumNestingDepth" }
    }

    val skipSpaces: (TextMatcher) -> Unit = if (slashSlashComment || slashStarComment)
        ::skipSpacesWithComments
    else
        ::skipSpacesSimple

    private fun skipSpacesSimple(tm: TextMatcher) {
        tm.skip(JSONFunctions::isSpaceCharacter)
    }

    private fun skipSpacesWithComments(tm: TextMatcher) {
        while (!tm.isAtEnd) {
            tm.skip(JSONFunctions::isSpaceCharacter)
            when {
                slashSlashComment && tm.match("//") -> {
                    tm.skip { it != '\n' && it != '\r' }
                }
                slashStarComment && tm.match("/*") -> {
                    tm.skipTo("*/")
                    if (!tm.match("*/"))
                        throw ParseException(UNCLOSED_COMMENT)
                }
                else -> break
            }
        }
    }

    companion object {
        val DEFAULT = ParseOptions()
    }

}
