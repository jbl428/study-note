package com.graphql.sample.book

import com.graphql.sample.book.dto.CreateBookInput
import org.springframework.graphql.data.method.annotation.Argument
import org.springframework.graphql.data.method.annotation.MutationMapping
import org.springframework.graphql.data.method.annotation.QueryMapping
import org.springframework.stereotype.Controller

@Controller
class BookController(private val bookService: BookService) {

    @QueryMapping
    suspend fun books() = bookService.find()

    @MutationMapping
    suspend fun createBook(@Argument input: CreateBookInput): Book = bookService.create(input)
}
