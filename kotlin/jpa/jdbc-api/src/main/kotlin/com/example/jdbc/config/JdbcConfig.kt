package com.example.jdbc.config

import com.example.entity.AuthorId
import org.springframework.context.annotation.Configuration
import org.springframework.core.convert.converter.Converter
import org.springframework.data.convert.ReadingConverter
import org.springframework.data.convert.WritingConverter
import org.springframework.data.jdbc.core.convert.JdbcCustomConversions
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration

@Configuration
class JdbcConfig : AbstractJdbcConfiguration() {

    override fun jdbcCustomConversions(): JdbcCustomConversions {
        return JdbcCustomConversions(
            listOf(
                AuthorIdReadingConverter(),
                AuthorIdWritingConverter()
            )
        )
    }

    @ReadingConverter
    class AuthorIdReadingConverter : Converter<Long, AuthorId> {
        override fun convert(source: Long): AuthorId = AuthorId(source)
    }

    @WritingConverter
    class AuthorIdWritingConverter : Converter<AuthorId, Long> {
        override fun convert(source: AuthorId): Long = source.value
    }
}
