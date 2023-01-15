package com.example.jpa.service

import com.example.entity.Post
import com.example.entity.type.PostType
import com.example.jpa.repository.PostRepository
import jakarta.transaction.Transactional
import org.springframework.stereotype.Service

@Service
class PostService(
    private val postRepository: PostRepository
) {

    @Transactional
    fun create() {
        val post = Post.of("title", "content", PostType.QUESTION)

        postRepository.save(post)
    }
}
