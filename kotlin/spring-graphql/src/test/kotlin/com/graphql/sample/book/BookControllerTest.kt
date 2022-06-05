package com.graphql.sample.book

import com.graphql.sample.book.dto.CreateBookInput
import com.ninjasquad.springmockk.MockkBean
import io.kotest.core.spec.style.StringSpec
import io.mockk.coEvery
import org.springframework.boot.test.autoconfigure.graphql.tester.AutoConfigureHttpGraphQlTester
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.graphql.test.tester.HttpGraphQlTester

@SpringBootTest
@AutoConfigureHttpGraphQlTester
internal class BookControllerTest(
    @MockkBean
    private val bookService: BookService,
    private val graphQlTester: HttpGraphQlTester
) : StringSpec({

    "find" {
        // given
        val book = Book("title", "author", "isbn").also { it.id = "1" }
        coEvery { bookService.find() } returns listOf(book)
        val authTester = graphQlTester
            .mutate()
            .headers { it.setBasicAuth("admin", "admin") }
            .build()

        // when
        val response = authTester
            .documentName("books")
            .execute()

        // then
        response
            .path("books")
            .entityList(Book::class.java)
            .hasSize(1)
            .contains(book)
    }

    "create" {
        // given
        val input = CreateBookInput("title", "author", "isbn")
        val book = Book("title", "author", "isbn").also { it.id = "1" }
        coEvery { bookService.create(any()) } returns book

        // when
        val response = graphQlTester
            .documentName("createBook")
            .variable("input", input)
            .execute()

        // then
        response
            .path("createBook")
            .entity(Book::class.java)
            .isEqualTo(book)
    }
})
