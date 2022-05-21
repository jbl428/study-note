package com.graphql.sample.book

import org.springframework.data.mongodb.core.mapping.Document

@Document
data class Book(val id: String, val title: String, val author: String, val isbn: String)
