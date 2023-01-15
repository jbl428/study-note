package com.example.jpa.repository

import com.example.entity.Post
import com.example.entity.type.PostType
import com.linecorp.kotlinjdsl.query.creator.CriteriaQueryCreatorImpl
import com.linecorp.kotlinjdsl.query.creator.SubqueryCreatorImpl
import com.linecorp.kotlinjdsl.spring.data.SpringDataQueryFactoryImpl
import jakarta.persistence.EntityManager
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest

@DataJpaTest
class PostQueryRepositoryTest @Autowired constructor(
    private val em: EntityManager
) {
    private val postQueryRepository = PostQueryRepository(
        SpringDataQueryFactoryImpl(
            criteriaQueryCreator = CriteriaQueryCreatorImpl(em),
            subqueryCreator = SubqueryCreatorImpl()
        )
    )

    @Test
    fun findByTitle() {
        val title = "title"
        em.persist(Post.of(title, "content", PostType.QUESTION))

        val posts = postQueryRepository.findByTitle(title)

        assertEquals(1, posts.size)
        assertEquals(title, posts.first().title)
    }
}
