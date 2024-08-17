/*
 * @(#) LookupSet.kt
 *
 * kjson-core  JSON Kotlin core functionality
 * Copyright (c) 2023 Peter Wall
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

package io.kjson.util

/**
 * Simple lookup set.  This is intended to allow the use of the `in {collection}` syntax with sets of values that either
 * (1) are too simple to justify using a `Set` (_e.g._ only 2 values), or (2) require algorithmic checking (_e.g._ an
 * `Int` is even &ndash; easy to check in an `if` but not easy in a `when` selector).
 *
 * The constructor takes a lambda parameter, a function that tests whether the value is a member of the notional "set".
 * It is expected that only the `contains` function will be used, mostly from the `in` or `!in` construct.
 *
 * @author  Peter Wall
 */
class LookupSet<T>(override val size: Int = 1, val check: (T) -> Boolean): Set<T> {

    // TODO - rename to PseudoSet? (name better represents its purpose)

    override fun isEmpty(): Boolean = false

    override fun iterator(): Iterator<T> = throw UnsupportedOperationException()

    override fun contains(element: T): Boolean = check(element)

    override fun containsAll(elements: Collection<T>): Boolean {
        for (element in elements)
            if (!contains(element))
                return false
        return true
    }

}
