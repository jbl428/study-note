package com.graphql.sample.book

import com.graphql.sample.book.dto.CreateBookInput
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.stereotype.Service

@Service
class BookService(private val bookRepository: BookRepository) {

    suspend fun create(input: CreateBookInput): Book =
        Book(
            title = input.title,
            author = input.author,
            isbn = input.isbn,
        ).run {
            bookRepository.save(this)
        }.awaitSingle()
}
