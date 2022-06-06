package com.graphql.sample.operation

import org.bson.Document
import org.springframework.data.mapping.toDotPath
import kotlin.reflect.KProperty

@DslMarker
annotation class SearchDsl

@SearchDsl
class SearchOperationBuilder {
    private val document = Document()

    var index: String? = null
        set(value) {
            document["index"] = value
            field = value
        }

    fun text(block: SearchTextBuilder.() -> Unit) {
        document["text"] = SearchTextBuilder().apply(block).build()
    }

    fun build(): SearchOperation = SearchOperation(document)
}

@SearchDsl
class SearchTextBuilder {
    private var query: List<String> = emptyList()
    private var path: List<String> = emptyList()

    fun query(vararg value: String) {
        query = value.toList()
    }

    fun path(block: SearchTextPathBuilder.() -> Unit) {
        path = SearchTextPathBuilder().apply(block).build()
    }

    fun build(): Document {
        check(query.isNotEmpty()) { "query 는 필수로 넣어야 합니다" }

        return Document("query", if (query.size == 1) query.first() else query)
            .append("path", path.ifEmpty { "*" })
    }
}

@SearchDsl
class SearchTextPathBuilder {
    private var path: MutableList<String> = mutableListOf()

    operator fun String.unaryPlus() {
        path += this
    }

    operator fun KProperty<*>.unaryPlus() {
        path += this.toDotPath()
    }

    infix fun KProperty<*>.dot(value: KProperty<String>) {
        path += "${this.toDotPath()}.${value.toDotPath()}"
    }

    fun build(): List<String> = path
}
