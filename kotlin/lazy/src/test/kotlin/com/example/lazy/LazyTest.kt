package com.example.lazy

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class LazyTest {
    @Test
    fun `내장 Lazy 테스트`() {
        // given
        class Foo {
            val bar: String by lazy {
                println("lazy")
                "bar"
            }

            init {
                println("init")
                println(bar)
            }
        }

        // when
        val foo = Foo()
        println(foo.bar)
    }

    @Test
    fun `Or 동작 테스트`() {
        // given
        val lazyA = Lazy {
            true
        }
        val lazyB = Lazy<Boolean> {
            throw Error("error")
        }

        // when
        val result = lazyA() || lazyB()

        // then
        assertTrue(result)
    }

    @Test
    fun `And 동작 테스트`() {
        // given
        val lazyA = Lazy {
            false
        }
        val lazyB = Lazy<Boolean> {
            throw Error("error")
        }

        // when
        val result = lazyA() && lazyB()

        // then
        assertFalse(result)
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
        val result = lazy.map { a -> a + 10 }

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
        val result = lazy.flatMap { a -> Lazy { a + 10 } }

        // then
        assertEquals(count, 0)
        assertEquals(result(), 20)
        assertEquals(count, 1)
        result()
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
        result()
        assertEquals(count, 2)
    }

    @Test
    fun `Sequence 동작 테스트`() {
        // given
        var count = 0
        val list = (1..10).map {
            Lazy {
                count++
                it
            }
        }

        // when
        val result = sequence(list)

        // then
        assertEquals(count, 0)
        assertEquals(result(), (1..10).toList())
        assertEquals(count, 10)
        result()
        assertEquals(count, 10)
    }
}
