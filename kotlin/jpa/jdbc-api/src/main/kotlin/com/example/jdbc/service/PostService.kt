package com.example.jdbc.service

import com.example.entity.AuthorId
import com.example.entity.Post
import com.example.entity.PostType
import com.example.jdbc.repository.PostRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
class PostService(
    private val postRepository: PostRepository
) {

    @Transactional
    fun create(): Post {
        val authorId = AuthorId(100)
        val post = Post.of("title", "content", PostType.QUESTION, authorId)

        return postRepository.save(post)
    }

    @Transactional
    fun shouldRollback(rollback: Boolean = true) {
        val post1 = Post.of("question", "question body", PostType.QUESTION, AuthorId(100))
        postRepository.save(post1)

        if (rollback) throw RuntimeException("error")

        val post2 = Post.of("answer", "answer body", PostType.ANSWER, AuthorId(200))
        postRepository.save(post2)
    }
}
