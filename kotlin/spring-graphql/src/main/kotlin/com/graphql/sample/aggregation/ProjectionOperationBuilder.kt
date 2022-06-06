package com.graphql.sample.aggregation

import org.springframework.data.mongodb.core.aggregation.Aggregation.project
import org.springframework.data.mongodb.core.aggregation.Fields
import org.springframework.data.mongodb.core.aggregation.ProjectionOperation
import kotlin.reflect.KProperty

@AggregationDsl
class ProjectionOperationBuilder {
    private var project = project()

    operator fun String.unaryPlus() {
        project = project.andInclude(Fields.fields(this))
    }

    operator fun String.unaryMinus() {
        project = project.andExclude(this)
    }

    operator fun KProperty<*>.unaryPlus() {
        project = project.andInclude(this.name)
    }

    operator fun KProperty<*>.unaryMinus() {
        project = project.andExclude(this.name)
    }

    fun excludeId() {
        project = project.andExclude("_id")
    }

    fun build(): ProjectionOperation = project
}
