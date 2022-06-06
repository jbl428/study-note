package com.graphql.sample.aggregation

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import org.springframework.data.domain.Sort
import org.springframework.data.mongodb.core.aggregation.Aggregation

internal class AggregationTest : StringSpec({

    "limit stage 를 생성한다" {
        val query = aggregation {
            limit(10)
        }

        query.toPipeline(Aggregation.DEFAULT_CONTEXT).map { it.toJson() } shouldBe
            listOf("""{"${"$"}limit": 10}""")
    }

    "search stage 를 생성한다" {
        val query = aggregation {
            search {
                text {
                    query("query")
                }
            }
        }

        query.toPipeline(Aggregation.DEFAULT_CONTEXT).map { it.toJson() } shouldBe
            listOf("""{"${"$"}search": {"text": {"query": "query", "path": "*"}}}""")
    }

    "projection stage 를 생성한다" {
        val query = aggregation {
            project {
                +"name"
            }
        }

        query.toPipeline(Aggregation.DEFAULT_CONTEXT).map { it.toJson() } shouldBe
            listOf("""{"${"$"}project": {"name": 1}}""")
    }

    "여러 stage 를 순서대로 생성한다" {
        val query = aggregation {
            stage(Aggregation.sort(Sort.Direction.DESC, "name"))
            project {
                +"include"
            }
            limit(5)
        }

        query.toPipeline(Aggregation.DEFAULT_CONTEXT).map { it.toJson() } shouldBe
            listOf(
                """{"${"$"}sort": {"name": -1}}""",
                """{"${"$"}project": {"include": 1}}""",
                """{"${"$"}limit": 5}""",
            )
    }
})
