package com.example.jdbc.service

import com.example.jdbc.repository.PostRepository
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.matchers.shouldBe
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.data.repository.findByIdOrNull
import java.lang.RuntimeException

@SpringBootTest
class PostServiceTest @Autowired constructor(
    private val postRepository: PostRepository,
    private val postService: PostService
) {

    @Test
    fun create() {
        // when
        val post = postService.create()

        // then
        val result = postRepository.findByIdOrNull(post.id)
        result shouldBe post
    }

    @Test
    fun shouldRollback() {
        // when
        shouldThrow<RuntimeException> {
            postService.shouldRollback()
        }

        // then
        val result = postRepository.findAll()
        result.count() shouldBe 0
    }
}
