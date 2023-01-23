package com.example.entity

import org.springframework.data.relational.core.mapping.Table

@Table
data class Post private constructor(
    val title: String,
    val content: String,
    val type: PostType,
    // val author: IdOnlyAggregateReference<User, Long>,
    val authorId: AuthorId
) : BaseEntity() {

    companion object {
        fun of(title: String, content: String, postType: PostType, authorId: AuthorId): Post =
            Post(
                title = title,
                content = content,
                type = postType,
                // author = IdOnlyAggregateReference(userId),
                authorId = authorId
            )
    }
}

@JvmInline
value class AuthorId(val value: Long)
