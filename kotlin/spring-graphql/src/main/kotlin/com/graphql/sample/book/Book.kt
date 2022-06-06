package com.graphql.sample.book

import org.springframework.data.annotation.Id

data class Book(
    val title: String,
    val author: String,
    val isbn: String,
    val publishers: List<Publisher> = emptyList(),
    val info: Info? = null
) {
    @Id
    lateinit var id: String
}

data class Publisher(
    val name: String,
    val email: String,
)

data class Info(
    val preface: String,
)
