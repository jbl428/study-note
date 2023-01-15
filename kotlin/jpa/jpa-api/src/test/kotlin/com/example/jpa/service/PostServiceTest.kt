package com.example.jpa.service

import com.example.jpa.repository.PostRepository
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest

@DataJpaTest
class PostServiceTest @Autowired constructor(
    private val postRepository: PostRepository
) {
    private val postService = PostService(postRepository)

    @Test
    fun create() {
        // when
        postService.create()

        // then
        val posts = postRepository.findAll()
        assert(posts.size == 1)
    }
}
