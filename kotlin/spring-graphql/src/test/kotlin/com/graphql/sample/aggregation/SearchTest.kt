package com.graphql.sample.aggregation

import com.graphql.sample.book.Book
import com.graphql.sample.book.Info
import com.graphql.sample.book.Publisher
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import org.springframework.data.mapping.div
import org.springframework.data.mongodb.core.aggregation.Aggregation

internal class SearchTest : StringSpec({

    fun search(block: SearchOperationBuilder.() -> Unit): SearchOperation =
        SearchOperationBuilder().apply(block).build()

    fun SearchOperation.toJson(): String = toPipelineStages(Aggregation.DEFAULT_CONTEXT).first().toJson()

    "index 필드를 세팅한다" {
        val operation = search {
            index = "default"
        }

        operation.toJson() shouldBe """
            {"${"$"}search": {"index": "default"}}
        """.trimIndent()
    }

    "query 필드를 세팅한다" {
        val operation = search {
            text {
                query("search", "body")
            }
        }

        operation.toJson() shouldBe """
            {"${"$"}search": {"text": {"query": ["search", "body"], "path": "*"}}}
        """.trimIndent()
    }

    "path 필드를 세팅한다" {
        val operation = search {
            text {
                query("search")
                path {
                    +"body"
                    +Book::title
                    +(Book::info / Info::preface)
                    Book::publishers dot Publisher::email
                }
            }
        }

        operation.toJson() shouldBe """
            {"${"$"}search": {"text": {"query": "search", "path": ["body", "title", "info.preface", "publishers.email"]}}}
        """.trimIndent()
    }

    "query 를 제공하지 않으면 에러가 발생한다" {
        val exception = shouldThrow<IllegalStateException> {
            search {
                text {
                    path { +"body" }
                }
            }
        }

        exception.message shouldBe "query 는 필수로 넣어야 합니다"
    }
})
