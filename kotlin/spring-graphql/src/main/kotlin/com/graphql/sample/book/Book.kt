package com.graphql.sample.book

import org.springframework.data.annotation.Id

data class Book(
    val title: String,
    val author: String,
    val isbn: String,
) {
    @Id
    lateinit var id: String
}
