package com.example.lazy

class Lazy<out A>(fn: () -> A) {
    private val value by lazy(fn)
    operator fun invoke(): A {
        return value
    }

    fun <B> map(fn: (A) -> B): Lazy<B> = Lazy { fn(value) }

    fun <B> flatMap(fn: (A) -> Lazy<B>): Lazy<B> = Lazy { fn(value)() }
}

fun or(a: Lazy<Boolean>, b: Lazy<Boolean>) = a() || b()

fun <A, B, C> map2(fn: (A, B) -> C, fa: Lazy<A>, fb: Lazy<B>): Lazy<C> = Lazy {
    fn(fa(), fb())
}

fun <A> sequence(list: List<Lazy<A>>): Lazy<List<A>> = Lazy { list.map { it() } }