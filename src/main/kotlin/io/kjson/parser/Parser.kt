/*
 * @(#) Parser.kt
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

package io.kjson.parser

import io.jstuff.json.JSONFunctions
import io.jstuff.text.TextMatcher

import io.kjson.JSONArray
import io.kjson.JSONBoolean
import io.kjson.JSONDecimal
import io.kjson.JSONException
import io.kjson.JSONInt
import io.kjson.JSONLong
import io.kjson.JSONNumber
import io.kjson.JSONObject
import io.kjson.JSONString
import io.kjson.JSONValue
import io.kjson.parser.ParserConstants.BOM
import io.kjson.parser.ParserConstants.MAX_INTEGER_DIGITS_LENGTH
import io.kjson.parser.ParserConstants.identifierContinuationSet
import io.kjson.parser.ParserConstants.identifierStartSet
import io.kjson.parser.ParserErrors.EXCESS_CHARS
import io.kjson.parser.ParserErrors.ILLEGAL_KEY
import io.kjson.parser.ParserErrors.ILLEGAL_NUMBER
import io.kjson.parser.ParserErrors.ILLEGAL_SYNTAX
import io.kjson.parser.ParserErrors.MAX_DEPTH_EXCEEDED
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACE
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACKET
import io.kjson.parser.ParserErrors.MISSING_COLON
import io.kjson.util.BuilderException

/**
 * A JSON parser.
 *
 * @author  Peter Wall
 */
object Parser {

    /**
     * Parse a [String] to a [JSONValue] (or `null` if the string is `"null"`).
     */
    fun parse(json: String, options: ParseOptions = ParseOptions.DEFAULT): JSONValue? {
        val tm = TextMatcher(json)
        tm.match(BOM) // skip BOM if present (not required, but may help interoperability)
        val result = parse(tm, options, 0)
        options.skipSpaces(tm)
        if (!tm.isAtEnd)
            throw ParseException(EXCESS_CHARS)
        return result
    }

    /**
     * Parse a [String] to a [JSONArray], where the input is in JSON Lines form.
     */
    fun parseLines(jsonLines: String, options: ParseOptions = ParseOptions.DEFAULT): JSONArray {
        val tm = TextMatcher(jsonLines)
        tm.match(BOM) // skip BOM if present (not required, but may help interoperability)
        return JSONArray.build {
            options.skipSpaces(tm)
            var counter = 0
            while (!tm.isAtEnd) {
                try {
                    val value = parse(tm, options, 0)
                    add(value)
                    counter++
                    options.skipSpaces(tm)
                }
                catch (pe: ParseException) {
                    throw pe.nested("/$counter")
                }
            }
        }
    }

    private fun parse(tm: TextMatcher, options: ParseOptions, depth: Int): JSONValue? {
        if (depth > options.maximumNestingDepth)
            throw JSONException(MAX_DEPTH_EXCEEDED)
        options.skipSpaces(tm)

        if (tm.match('{'))
            return parseObject(tm, options, depth)

        if (tm.match('['))
            return parseArray(tm, options, depth)

        if (tm.match('"'))
            return JSONString(parseString(tm))

        if (tm.match("true"))
            return JSONBoolean.TRUE

        if (tm.match("false"))
            return JSONBoolean.FALSE

        if (tm.match("null"))
            return null

        return parseNumber(tm) ?: throw ParseException(ILLEGAL_SYNTAX)
    }

    private fun parseObject(
        tm: TextMatcher,
        options: ParseOptions,
        depth: Int,
    ) = JSONObject.build(duplicateKeyOption = options.objectKeyDuplicate) {
        options.skipSpaces(tm)
        if (!tm.match('}')) {
            while (true) {
                val key = when {
                    tm.match('"') -> parseString(tm)
                    options.objectKeyUnquoted && tm.matchIdentifier() -> tm.result
                    else -> throw ParseException(ILLEGAL_KEY)
                }
                try {
                    options.skipSpaces(tm)
                    if (!tm.match(':'))
                        throw ParseException(MISSING_COLON)
                    val value = parse(tm, options, depth + 1)
                    try {
                        add(key, value)
                    }
                    catch (e: BuilderException) {
                        throw ParseException(e.text)
                    }
                    options.skipSpaces(tm)
                    if (!tm.match(','))
                        break
                    options.skipSpaces(tm)
                }
                catch (pe: ParseException) {
                    throw pe.nested("/$key")
                }
                if (options.objectTrailingComma && tm.match('}'))
                    return@build
            }
            if (!tm.match('}'))
                throw ParseException(MISSING_CLOSING_BRACE)
        }
    }

    private fun parseArray(
        tm: TextMatcher,
        options: ParseOptions,
        depth: Int,
    ) = JSONArray.build {
        options.skipSpaces(tm)
        if (!tm.match(']')) {
            while (true) {
                try {
                    val value = parse(tm, options, depth + 1)
                    add(value)
                    options.skipSpaces(tm)
                    if (!tm.match(','))
                        break
                    options.skipSpaces(tm)
                }
                catch (pe: ParseException) {
                    throw pe.nested("/$size")
                }
                if (options.arrayTrailingComma && tm.match(']'))
                    return@build
            }
            if (!tm.match(']'))
                throw ParseException(MISSING_CLOSING_BRACKET)
        }
    }

    private fun parseString(tm: TextMatcher): String = try {
        JSONFunctions.parseString(tm)
    } catch (iae: IllegalArgumentException) {
        throw ParseException(iae.message ?: "Error parsing JSON string")
    }

    private fun parseNumber(tm: TextMatcher): JSONNumber? {
        val numberStart = tm.index
        val negative = tm.match('-')
        if (tm.matchDec()) {
            val integerLength = tm.resultLength
            when {
                integerLength > 1 && tm.resultChar == '0' -> throw ParseException(ILLEGAL_NUMBER)
                tm.match('.') -> {
                    if (!tm.matchDec())
                        throw ParseException(ILLEGAL_NUMBER)
                    skipExponent(tm)
                }
                !skipExponent(tm) -> {
                    // no decimal point or "e"/"E" - try JSONInt or JSONLong
                    if (integerLength < MAX_INTEGER_DIGITS_LENGTH)
                        return JSONInt.of(tm.getResultInt(negative))
                    try {
                        val result = tm.getResultLong(negative)
                        return if (result >= Int.MIN_VALUE && result <= Int.MAX_VALUE)
                            JSONInt.of(result.toInt())
                        else
                            JSONLong.of(result)
                    }
                    catch (_: NumberFormatException) {
                        // too big for long - drop through to BigDecimal
                    }
                }
            }
            return JSONDecimal.of(tm.getString(numberStart, tm.index).toBigDecimal())
        }
        return null
    }

    private fun skipExponent(tm: TextMatcher): Boolean = if (tm.match { it == 'e' || it == 'E' }) {
        tm.match { it == '-' || it == '+' } // ignore the result, just step the index
        if (!tm.matchDec())
            throw ParseException(ILLEGAL_NUMBER)
        true
    } else
        false

    private fun TextMatcher.matchIdentifier(): Boolean = match { it in identifierStartSet } &&
            matchContinue { it in identifierContinuationSet }

}
