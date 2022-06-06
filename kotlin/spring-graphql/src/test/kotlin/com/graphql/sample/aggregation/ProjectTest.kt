package com.graphql.sample.aggregation

import com.graphql.sample.book.Book
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import org.springframework.data.mongodb.core.aggregation.Aggregation
import org.springframework.data.mongodb.core.aggregation.ProjectionOperation

internal class ProjectTest : StringSpec({

    fun project(block: ProjectionOperationBuilder.() -> Unit): ProjectionOperation =
        ProjectionOperationBuilder().apply(block).build()

    fun ProjectionOperation.toJson(): String = toPipelineStages(Aggregation.DEFAULT_CONTEXT).first().toJson()

    "include" {
        val operation = project {
            +"name"
            +Book::author
        }

        operation.toJson() shouldBe """
            {"${"$"}project": {"name": 1, "author": 1}}
        """.trimIndent()
    }

    "exclude" {
        val operation = project {
            -"name"
            -Book::author
        }

        operation.toJson() shouldBe """
            {"${"$"}project": {"name": 0, "author": 0}}
        """.trimIndent()
    }

    "exclude id" {
        val operation = project {
            excludeId()
        }

        operation.toJson() shouldBe """
            {"${"$"}project": {"_id": 0}}
        """.trimIndent()
    }
})
