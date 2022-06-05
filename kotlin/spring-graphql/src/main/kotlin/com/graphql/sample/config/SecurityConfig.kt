package com.graphql.sample.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.ServerHttpSecurity
import org.springframework.security.config.web.server.invoke
import org.springframework.security.core.userdetails.MapReactiveUserDetailsService
import org.springframework.security.core.userdetails.User
import org.springframework.security.core.userdetails.UserDetails
import org.springframework.security.web.server.SecurityWebFilterChain

@Configuration
@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
class SecurityConfig {

    @Bean
    fun springWebFilterChain(http: ServerHttpSecurity): SecurityWebFilterChain =
        http {
            csrf { disable() }
            authorizeExchange {
                authorize(anyExchange, permitAll)
            }
            httpBasic {}
        }

    @Bean
    fun userDetailsService(): MapReactiveUserDetailsService {
        val userBuilder = User.withDefaultPasswordEncoder()
        val rob: UserDetails = userBuilder.username("rob").password("rob").roles("USER").build()
        val admin: UserDetails = userBuilder.username("admin").password("admin").roles("USER", "ADMIN").build()
        return MapReactiveUserDetailsService(rob, admin)
    }
}
