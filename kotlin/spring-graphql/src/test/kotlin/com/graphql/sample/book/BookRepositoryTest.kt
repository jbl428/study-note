package com.graphql.sample.book

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest

@DataMongoTest
internal class BookRepositoryTest(
    private val bookRepository: BookRepository,
) : StringSpec({

    beforeEach {
        bookRepository.deleteAll().block()
    }

    "저장 및 조회" {
        // given
        val book = Book(
            title = "제목",
            author = "저자",
            isbn = "978-4-7741-8411-1",
        )

        // when
        bookRepository.save(book).awaitSingle()

        // then
        val result = bookRepository.findById(book.id).awaitSingle()
        result shouldBe book
    }

    "개수 조회" {
        // given
        val book = Book(
            title = "제목",
            author = "저자",
            isbn = "978-4-7741-8411-1",
        )

        // when
        bookRepository.save(book).awaitSingle()

        // then
        val result = bookRepository.count().awaitSingle()
        result shouldBe 1
    }
})
