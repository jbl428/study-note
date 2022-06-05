package com.graphql.sample.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.authentication.ReactiveAuthenticationManager
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.SecurityWebFiltersOrder
import org.springframework.security.config.web.server.ServerHttpSecurity
import org.springframework.security.config.web.server.invoke
import org.springframework.security.core.authority.SimpleGrantedAuthority
import org.springframework.security.web.server.SecurityWebFilterChain
import org.springframework.security.web.server.authentication.AuthenticationWebFilter
import reactor.kotlin.core.publisher.toMono

@Configuration
@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
class SecurityConfig {

    @Bean
    fun springWebFilterChain(
        http: ServerHttpSecurity,
        manager: AuthenticationManager,
    ): SecurityWebFilterChain =
        http {
            csrf { disable() }
            authorizeExchange {
                authorize(anyExchange, authenticated)
            }
            httpBasic { disable() }
            addFilterAt(jwtAuthenticationFilter(manager), SecurityWebFiltersOrder.AUTHENTICATION)
        }

    fun jwtAuthenticationFilter(manager: ReactiveAuthenticationManager): AuthenticationWebFilter {
        val bearerAuthenticationFilter = AuthenticationWebFilter(manager)

        bearerAuthenticationFilter.setServerAuthenticationConverter {
            UsernamePasswordAuthenticationToken(
                "",
                "",
                listOf(SimpleGrantedAuthority("ROLE_ADMIN"))
            ).toMono()
        }

        return bearerAuthenticationFilter
    }
}
