package com.graphql.sample.search

import com.graphql.sample.book.Book
import com.graphql.sample.book.Info
import com.graphql.sample.book.Publisher
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import org.springframework.data.mapping.div
import org.springframework.data.mongodb.core.aggregation.Aggregation

internal class SearchTest : StringSpec({

    "index 필드를 세팅한다" {
        val searchOperation =
            search {
                index = "default"
            }

        searchOperation.json shouldBe """
            {"${"$"}search": {"index": "default"}}
        """.trimIndent()
    }

    "query 필드를 세팅한다" {
        val searchOperation =
            search {
                text {
                    query("search", "body")
                }
            }

        searchOperation.json shouldBe """
            {"${"$"}search": {"text": {"query": ["search", "body"], "path": "*"}}}
        """.trimIndent()
    }

    "path 필드를 세팅한다" {
        val searchOperation =
            search {
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

        searchOperation.json shouldBe """
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

val SearchOperation.json: String
    get() = toPipelineStages(Aggregation.DEFAULT_CONTEXT).first().toJson()
