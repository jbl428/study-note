package com.example.jdbc.service

import com.example.jdbc.repository.PostRepository
import io.kotest.matchers.shouldBe
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.data.jdbc.DataJdbcTest
import org.springframework.data.repository.findByIdOrNull

@DataJdbcTest
class PostServiceTest @Autowired constructor(
    private val postRepository: PostRepository
) {
    private val postService = PostService(postRepository)

    @Test
    fun create() {
        // when
        val post = postService.create()

        // then
        val result = postRepository.findByIdOrNull(post.id)
        result shouldBe post
    }
}
