/*
 * @(#) Parser.kt
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

package io.kjson.parser

import java.math.BigDecimal

import io.kjson.JSONArray
import io.kjson.JSONBoolean
import io.kjson.JSONDecimal
import io.kjson.JSONInt
import io.kjson.JSONLong
import io.kjson.JSONObject
import io.kjson.JSONString
import io.kjson.JSONValue

import net.pwall.json.JSONFunctions
import net.pwall.text.TextMatcher

object Parser {

    const val rootPointer = ""

    private const val MAX_INTEGER_DIGITS_LENGTH = 10

    const val EXCESS_CHARS = "Excess characters following JSON"
    const val ILLEGAL_NUMBER = "Illegal JSON number"
    const val ILLEGAL_SYNTAX = "Illegal JSON syntax"
    const val ILLEGAL_KEY = "Illegal key in JSON object"
    const val DUPLICATE_KEY = "Duplicate key in JSON object"
    const val MISSING_COLON = "Missing colon in JSON object"
    const val MISSING_CLOSING_BRACE = "Missing closing brace in JSON object"
    const val MISSING_CLOSING_BRACKET = "Missing closing bracket in JSON array"

    fun parse(json: String): JSONValue? {
        val tm = TextMatcher(json)
        val result = parse(tm, rootPointer)
        tm.skip(JSONFunctions::isSpaceCharacter)
        if (!tm.isAtEnd)
            throw ParseException(EXCESS_CHARS)
        return result
    }

    private fun parse(tm: TextMatcher, pointer: String): JSONValue? {
        tm.skip(JSONFunctions::isSpaceCharacter)

        if (tm.match('{')) {
            val builder = JSONObject.Builder()
            tm.skip(JSONFunctions::isSpaceCharacter)
            if (!tm.match('}')) {
                while (true) {
                    if (!tm.match('"'))
                        throw ParseException(ILLEGAL_KEY, pointer)
                    val key = parseString(tm, pointer)
                    if (builder.containsKey(key))
                        throw ParseException(DUPLICATE_KEY, pointer)
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(':'))
                        throw ParseException(MISSING_COLON, pointer)
                    builder.add(key, parse(tm, "$pointer/$key"))
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(','))
                        break
                    tm.skip(JSONFunctions::isSpaceCharacter)
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
                    builder.add(parse(tm, "$pointer/${builder.size}"))
                    tm.skip(JSONFunctions::isSpaceCharacter)
                    if (!tm.match(','))
                        break
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
        if (tm.matchDec(0, 1)) {
            val integerLength = tm.resultLength
            if (integerLength > 1 && tm.resultChar == '0')
                throw ParseException(ILLEGAL_NUMBER, pointer)
            var floating = false
            if (tm.match('.')) {
                floating = true
                if (!tm.matchDec(0, 1))
                    throw ParseException(ILLEGAL_NUMBER, pointer)
            }
            if (tm.match('e') ||tm.match('E')) {
                floating = true
                tm.matchAny("-+") // ignore the result, just step the index
                if (!tm.matchDec(0, 1))
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
                catch (ignore: NumberFormatException) {
                    // too big for long - drop through to BigDecimal
                }
            }
            return JSONDecimal.of(BigDecimal(tm.getString(numberStart, tm.index)))
        }

        throw ParseException(ILLEGAL_SYNTAX, pointer)
    }

    private fun parseString(tm: TextMatcher, pointer: String): String = try {
            JSONFunctions.parseString(tm)
        }
        catch (iae: IllegalArgumentException) {
            throw ParseException(iae.message ?: "Error parsing JSON string", pointer)
        }

}
