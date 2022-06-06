package com.graphql.sample.aggregation

import org.springframework.data.mongodb.core.aggregation.Aggregation
import org.springframework.data.mongodb.core.aggregation.AggregationOperation

@DslMarker
annotation class AggregationDsl

@AggregationDsl
class AggregationBuilder {
    private val operations: MutableList<AggregationOperation> = mutableListOf()

    fun search(block: SearchOperationBuilder.() -> Unit) {
        operations += SearchOperationBuilder().apply(block).build()
    }

    fun stage(operation: AggregationOperation) {
        operations += operation
    }

    fun limit(limit: Long) {
        operations += Aggregation.limit(limit)
    }

    fun project(block: ProjectionOperationBuilder.() -> Unit) {
        operations += ProjectionOperationBuilder().apply(block).build()
    }

    fun build() = Aggregation.newAggregation(operations)
}

fun aggregation(block: AggregationBuilder.() -> Unit) =
    AggregationBuilder().apply(block).build()
