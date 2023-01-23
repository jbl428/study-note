package com.example.entity

import org.springframework.data.relational.core.mapping.Table

@Table
data class User(
    val name: String
) : BaseEntity()
