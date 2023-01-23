package com.example.entity

import org.springframework.data.annotation.CreatedDate
import org.springframework.data.annotation.Id
import org.springframework.data.annotation.LastModifiedDate
import org.springframework.data.domain.Persistable
import java.time.LocalDateTime

open class BaseEntity : Persistable<Long> {
    @Id
    var id: Long = 0L
        protected set

    @CreatedDate
    lateinit var createdAt: LocalDateTime
        protected set

    @LastModifiedDate
    lateinit var updatedAt: LocalDateTime
        protected set

    override fun getId(): Long = id
    override fun isNew(): Boolean = id == 0L
}
