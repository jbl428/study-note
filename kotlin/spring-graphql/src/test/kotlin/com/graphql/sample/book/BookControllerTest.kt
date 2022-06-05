package com.graphql.sample.book

import com.graphql.sample.book.dto.CreateBookInput
import com.ninjasquad.springmockk.MockkBean
import io.kotest.core.spec.style.StringSpec
import io.mockk.coEvery
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.ApplicationContext
import org.springframework.graphql.test.tester.HttpGraphQlTester
import org.springframework.test.web.reactive.server.WebTestClient

@SpringBootTest
internal class BookControllerTest(
    @MockkBean
    private val bookService: BookService,
    private val context: ApplicationContext,
) : StringSpec({
    lateinit var graphQlTester: HttpGraphQlTester

    beforeSpec {
        val client = WebTestClient
            .bindToApplicationContext(context)
            .configureClient()
            .baseUrl("/graphql")
            .build()

        graphQlTester = HttpGraphQlTester.create(client)
    }

    "find" {
        // given
        val book = Book("title", "author", "isbn").also { it.id = "1" }
        coEvery { bookService.find() } returns listOf(book)

        // when
        val response = graphQlTester
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
