package com.example.jpa

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.autoconfigure.domain.EntityScan
import org.springframework.boot.runApplication

@SpringBootApplication
@EntityScan(basePackages = ["com.example.entity"])
class JpaApplication

fun main(args: Array<String>) {
    runApplication<JpaApplication>(*args)
}
