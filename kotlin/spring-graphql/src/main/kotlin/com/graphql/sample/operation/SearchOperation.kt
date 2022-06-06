package com.graphql.sample.operation

import org.bson.Document
import org.springframework.data.mongodb.core.aggregation.AggregationOperation
import org.springframework.data.mongodb.core.aggregation.AggregationOperationContext

class SearchOperation(
    private val document: Document
) : AggregationOperation {

    override fun toDocument(context: AggregationOperationContext): Document =
        Document(operator, document)

    override fun getOperator(): String = "\$search"
}

fun search(block: SearchOperationBuilder.() -> Unit): SearchOperation {
    return SearchOperationBuilder().apply(block).build()
}
