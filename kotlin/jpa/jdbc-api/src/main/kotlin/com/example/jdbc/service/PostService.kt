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
}
