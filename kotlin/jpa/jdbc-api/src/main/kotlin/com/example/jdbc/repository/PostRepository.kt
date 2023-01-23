package com.example.jdbc.repository

import com.example.entity.Post
import org.springframework.data.repository.CrudRepository

interface PostRepository : CrudRepository<Post, Long>
