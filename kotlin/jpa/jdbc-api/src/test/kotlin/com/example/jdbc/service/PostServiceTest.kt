package com.example.jdbc.service

import com.example.jdbc.repository.PostRepository
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.data.jdbc.DataJdbcTest

@DataJdbcTest
class PostServiceTest @Autowired constructor(
    postRepository: PostRepository
) {
    private val postService = PostService(postRepository)

    @Test
    fun create() {
        // when
        val post = postService.create()

        // then
        println(post)
    }
}
