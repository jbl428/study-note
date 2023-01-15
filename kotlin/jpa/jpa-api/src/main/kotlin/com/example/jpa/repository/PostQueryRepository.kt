package com.example.jpa.repository

import com.example.entity.Post
import com.linecorp.kotlinjdsl.querydsl.expression.col
import com.linecorp.kotlinjdsl.spring.data.SpringDataQueryFactory
import com.linecorp.kotlinjdsl.spring.data.listQuery

class PostQueryRepository(
    private val queryFactory: SpringDataQueryFactory
) {

    fun findByTitle(title: String): List<Post> = queryFactory.listQuery {
        select(entity(Post::class))
        from(Post::class)
        where(col(Post::title).equal(title))
    }
}
