package com.example.lazy

class Lazy<out A>(fn: () -> A) {
    private val value by lazy(fn)
    operator fun invoke(): A {
        return value
    }
}

fun or(a: Lazy<Boolean>, b: Lazy<Boolean>) = a() || b()

fun <A, B> map(fn: (A) -> B, fa: Lazy<A>): Lazy<B> = Lazy { fn(fa()) }
