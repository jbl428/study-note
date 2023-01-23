package com.example.entity

import org.springframework.data.jdbc.core.mapping.AggregateReference.IdOnlyAggregateReference
import org.springframework.data.relational.core.mapping.Table

@Table
data class Post private constructor(
    val title: String,
    val content: String,
    val type: PostType,
    val author: IdOnlyAggregateReference<User, Long>
) : BaseEntity() {

    companion object {
        fun of(title: String, content: String, postType: PostType, userId: Long): Post =
            Post(title = title, content = content, type = postType, author = IdOnlyAggregateReference(userId))
    }
}
