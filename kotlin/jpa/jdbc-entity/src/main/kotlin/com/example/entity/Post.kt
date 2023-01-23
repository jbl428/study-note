package com.example.entity

import org.springframework.data.relational.core.mapping.Table

@Table
data class Post(
    val title: String,
    val content: String,
    val type: PostType,
    val authorId: AuthorId
    // val author: AggregateReference<Post, Long>,
) : BaseEntity() {

    companion object {
        fun of(title: String, content: String, postType: PostType, authorId: AuthorId): Post =
            Post(
                title = title,
                content = content,
                type = postType,
                authorId = authorId
                // author = AggregateReference.to(authorId),
            )
    }
}

data class AuthorId(val value: Long)
