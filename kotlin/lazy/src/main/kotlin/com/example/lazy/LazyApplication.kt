package com.example.lazy

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class LazyApplication

fun main(args: Array<String>) {
    runApplication<LazyApplication>(*args)
}
