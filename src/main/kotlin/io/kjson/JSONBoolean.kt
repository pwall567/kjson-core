/*
 * @(#) JSONBoolean.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2021, 2022, 2023 Peter Wall
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

/**
 * A JSON boolean value.
 *
 * @author  Peter Wall
 */
enum class JSONBoolean(override val value: Boolean) : JSONPrimitive<Boolean> {

    TRUE(true),
    FALSE(false);

    /**
     * Convert to a JSON string.
     */
    override fun toJSON(): String = if (value) "true" else "false"

    /**
     * Append as a JSON string to an [Appendable].
     */
    override fun appendTo(a: Appendable) {
        a.append(toJSON())
    }

    /**
     * Convert the value to [String].
     */
    override fun toString(): String = toJSON()

    /** The value as a [Boolean] (optimisation of the extension value in [JSON] when the type is known statically). */
    val asBoolean: Boolean
        get() = value

    /** The value as a [Boolean] or `null` (optimisation of the extension value in [JSON] when the type is known
     *  statically). */
    val asBooleanOrNull: Boolean
        get() = value

    companion object {

        /**
         * Create a [JSONBoolean] from a [Boolean].
         */
        fun of(b: Boolean): JSONBoolean = if (b) TRUE else FALSE

    }

}
