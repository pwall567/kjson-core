/*
 * @(#) JSONValue.kt
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

import java.util.function.IntConsumer

import io.kjson.JSON.accept
import net.pwall.util.CoOutput
import net.pwall.util.output

/**
 * Interface to represent a JSON value.
 *
 * @author  Peter Wall
 */
sealed interface JSONValue {

    /**
     * Append as a JSON string to an [Appendable].
     */
    fun appendTo(a: Appendable)

    /**
     * Convert to a JSON string.
     */
    fun toJSON(): String = buildString { appendTo(this) }

    /**
     * Output as a JSON string to an [IntConsumer].
     */
    fun output(out: IntConsumer) = out.accept(toJSON())

    /**
     * Output as a JSON string to a [CoOutput].
     */
    suspend fun coOutput(out: CoOutput) = out.output(toJSON())

}
