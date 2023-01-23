package com.example.jdbc

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.data.jdbc.repository.config.EnableJdbcAuditing
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories
import org.springframework.transaction.annotation.EnableTransactionManagement

@EnableJdbcAuditing
@EnableJdbcRepositories
@EnableTransactionManagement
@SpringBootApplication
class JdbcApplication

fun main(args: Array<String>) {
    runApplication<JdbcApplication>(*args)
}
