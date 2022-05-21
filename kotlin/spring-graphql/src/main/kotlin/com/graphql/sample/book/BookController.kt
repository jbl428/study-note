package com.graphql.sample.book

import org.springframework.graphql.data.method.annotation.QueryMapping
import org.springframework.stereotype.Controller

@Controller
class BookController {

    @QueryMapping
    suspend fun hello(): String = "test"
}
