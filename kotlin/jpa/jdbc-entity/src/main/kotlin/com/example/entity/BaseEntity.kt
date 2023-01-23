package com.example.entity

import org.springframework.data.annotation.Id
import org.springframework.data.domain.Persistable

open class BaseEntity : Persistable<Long> {
    @Id
    var id: Long = 0L
        protected set

    override fun getId(): Long = id
    override fun isNew(): Boolean = id == 0L
}
