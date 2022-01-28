package com.example.lazy

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class LazyTest {
    @Test
    fun `Lazy 동작 테스트`() {
        // given
        var count = 0
        val lazyA = Lazy {
            count++
            true
        }
        val lazyB = Lazy<Boolean> {
            throw Error("error")
        }

        // when
        lazyA() || lazyB()
        or(lazyA, lazyB)

        // then
        assertEquals(1, count)
    }

    @Test
    fun `Map 동작 테스트`() {
        // given
        var count = 0
        val lazy = Lazy {
            count++
            10
        }

        // when
        val result = map({ a -> a + 10 }, lazy)

        // then
        assertEquals(count, 0)
        assertEquals(result(), 20)
        assertEquals(count, 1)
    }

    @Test
    fun `FlatMap 동작 테스트`() {
        // given
        var count = 0
        val lazy = Lazy {
            count++
            10
        }

        // when
        val result = flatMap({ a -> Lazy { a + 10 } }, lazy)

        // then
        assertEquals(count, 0)
        assertEquals(result(), 20)
        assertEquals(count, 1)
    }

    @Test
    fun `Map2 동작 테스트`() {
        // given
        var count = 0
        val lazyA = Lazy {
            count++
            10
        }
        val lazyB = Lazy {
            count++
            20
        }
        fun add(a: Int, b: Int): Int = a + b

        // when
        val result = map2(::add, lazyA, lazyB)

        // then
        assertEquals(count, 0)
        assertEquals(result(), 30)
        assertEquals(count, 2)
    }
}
