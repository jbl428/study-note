package com.graphql.sample.book

import com.graphql.sample.book.dto.CreateBookInput
import io.kotest.assertions.assertSoftly
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest

@DataMongoTest
internal class BookServiceTest(
    private val bookRepository: BookRepository,
) : FreeSpec({
    val bookService = BookService(bookRepository)

    beforeSpec {
        bookRepository.deleteAll().block()
    }

    "find" {
        // given
        val book = Book(
            title = "The Lord of the Rings",
            author = "J.R.R. Tolkien",
            isbn = "0-395-07477-1",
        )
        bookRepository.save(book).awaitSingle()

        // when
        val foundBook = bookService.find()

        // then
        foundBook shouldBe listOf(book)
    }

    "create" {
        // given
        val input = CreateBookInput(
            title = "The Lord of the Rings",
            author = "J.R.R. Tolkien",
            isbn = "0-395-07477-1",
        )

        // when
        val result = bookService.create(input)

        // then
        assertSoftly(result) {
            title shouldBe input.title
            author shouldBe input.author
            isbn shouldBe input.isbn
        }
    }
})
