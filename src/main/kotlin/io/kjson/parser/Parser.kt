/*
 * @(#) Parser.kt
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

package io.kjson.parser

import io.kjson.JSONArray
import io.kjson.JSONBoolean
import io.kjson.JSONDecimal
import io.kjson.JSONInt
import io.kjson.JSONLong
import io.kjson.JSONObject
import io.kjson.JSONString
import io.kjson.JSONValue
import io.kjson.parser.ParserConstants.BOM
import io.kjson.parser.ParserConstants.MAX_INTEGER_DIGITS_LENGTH
import io.kjson.parser.ParserConstants.identifierContinuationSet
import io.kjson.parser.ParserConstants.identifierStartSet
import io.kjson.parser.ParserConstants.rootPointer
import io.kjson.parser.ParserErrors.DUPLICATE_KEY
import io.kjson.parser.ParserErrors.EXCESS_CHARS
import io.kjson.parser.ParserErrors.ILLEGAL_KEY
import io.kjson.parser.ParserErrors.ILLEGAL_NUMBER
import io.kjson.parser.ParserErrors.ILLEGAL_SYNTAX
import io.kjson.parser.ParserErrors.MAX_DEPTH_EXCEEDED
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACE
import io.kjson.parser.ParserErrors.MISSING_CLOSING_BRACKET
import io.kjson.parser.ParserErrors.MISSING_COLON
import net.pwall.json.JSONFunctions
import net.pwall.text.TextMatcher

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
        val result = parse(tm, options, rootPointer, 0)
        tm.skip(JSONFunctions::isSpaceCharacter)
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
            tm.skip(JSONFunctions::isSpaceCharacter)
            var counter = 0
            while (!tm.isAtEnd) {
                add(parse(tm, options, "/${counter++}", 0))
                tm.skip(JSONFunctions::isSpaceCharacter)
            }
        }
    }

    private fun parse(tm: TextMatcher, options: ParseOptions, pointer: String, depth: Int): JSONValue? {
        if (depth > options.maximumNestingDepth)
            throw ParseException(MAX_DEPTH_EXCEEDED)

        tm.skip(JSONFunctions::isSpaceCharacter)

        if (tm.match('{')) {
            val builder = JSONObject.Builder()
            tm.skip(JSONFunctions::isSpaceCharacter)
            if (!tm.match('}')) {
                while (true) {
                    val key = when {
                        tm.match('"') -> parseString(tm, pointer)
                        options.objectKeyUnquoted && tm.matchIdentifier() -> tm.result
                        else -> throw ParseException(ILLEGAL_KEY, pointer)
                    }
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(':'))
                        throw ParseException(MISSING_COLON, pointer)
                    val value = parse(tm, options, "$pointer/$key", depth + 1)
                    if (builder.containsKey(key)) {
                        when (options.objectKeyDuplicate) {
                            ParseOptions.DuplicateKeyOption.ERROR -> duplicateKeyError(key, pointer)
                            ParseOptions.DuplicateKeyOption.TAKE_FIRST -> {}
                            ParseOptions.DuplicateKeyOption.TAKE_LAST -> {
                                builder.remove(key)
                                builder.add(key, value)
                            }
                            ParseOptions.DuplicateKeyOption.CHECK_IDENTICAL -> {
                                if (value != builder.get(key))
                                    duplicateKeyError(key, pointer)
                            }
                        }
                    }
                    else
                        builder.add(key, value)
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(','))
                        break
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (options.objectTrailingComma && tm.match('}')) {
                        tm.revert()
                        break
                    }
                }
                if (!tm.match('}'))
                    throw ParseException(MISSING_CLOSING_BRACE, pointer)
            }
            return builder.build()
        }

        if (tm.match('[')) {
            val builder = JSONArray.Builder()
            tm.skip(JSONFunctions::isSpaceCharacter)
            if (!tm.match(']')) {
                while (true) {
                    builder.add(parse(tm, options, "$pointer/${builder.size}", depth + 1))
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(','))
                        break
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (options.arrayTrailingComma && tm.match(']')) {
                        tm.revert()
                        break
                    }
                }
                if (!tm.match(']'))
                    throw ParseException(MISSING_CLOSING_BRACKET, pointer)
            }
            return builder.build()
        }

        if (tm.match('"'))
            return JSONString(parseString(tm, pointer))

        if (tm.match("true"))
            return JSONBoolean.TRUE

        if (tm.match("false"))
            return JSONBoolean.FALSE

        if (tm.match("null"))
            return null

        val numberStart = tm.index
        val negative = tm.match('-')
        if (tm.matchDec()) {
            val integerLength = tm.resultLength
            if (integerLength > 1 && tm.resultChar == '0')
                throw ParseException(ILLEGAL_NUMBER, pointer)
            var floating = false
            if (tm.match('.')) {
                floating = true
                if (!tm.matchDec())
                    throw ParseException(ILLEGAL_NUMBER, pointer)
            }
            if (tm.match { it == 'e' || it == 'E' }) {
                floating = true
                tm.match { it == '-' || it == '+' } // ignore the result, just step the index
                if (!tm.matchDec())
                    throw ParseException(ILLEGAL_NUMBER, pointer)
            }
            if (!floating) {
                if (integerLength < MAX_INTEGER_DIGITS_LENGTH)
                    return JSONInt.of(tm.getResultInt(negative))
                try {
                    val result = tm.getResultLong(negative)
                    if (result >= Int.MIN_VALUE && result <= Int.MAX_VALUE)
                        return JSONInt.of(result.toInt())
                    return JSONLong.of(result)
                }
                catch (_: NumberFormatException) {
                    // too big for long - drop through to BigDecimal
                }
            }
            return JSONDecimal.of(tm.getString(numberStart, tm.index).toBigDecimal())
        }

        throw ParseException(ILLEGAL_SYNTAX, pointer)
    }

    private fun parseString(tm: TextMatcher, pointer: String): String = try {
            JSONFunctions.parseString(tm)
        }
        catch (iae: IllegalArgumentException) {
            throw ParseException(iae.message ?: "Error parsing JSON string", pointer)
        }

    private fun duplicateKeyError(key: String, pointer: String): Nothing {
        throw ParseException("$DUPLICATE_KEY \"$key\"", pointer)
    }

    private fun TextMatcher.matchIdentifier(): Boolean = match { it in identifierStartSet } &&
            matchContinue { it in identifierContinuationSet }

}
