package com.graphql.sample.book.dto

data class CreateBookInput(
    val title: String,
    val author: String,
    val isbn: String,
)
