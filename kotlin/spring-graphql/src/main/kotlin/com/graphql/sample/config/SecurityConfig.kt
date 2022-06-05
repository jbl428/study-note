package com.graphql.sample.config

import arrow.core.firstOrNone
import arrow.core.flatMap
import io.github.nefilim.kjwt.JWSHMAC256Algorithm
import io.github.nefilim.kjwt.KJWTVerificationError
import io.github.nefilim.kjwt.verifySignature
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.http.HttpHeaders
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
import reactor.core.publisher.Mono
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
                authorize(anyExchange, permitAll)
            }
            httpBasic { disable() }
            addFilterAt(jwtAuthenticationFilter(manager), SecurityWebFiltersOrder.AUTHENTICATION)
        }

    fun jwtAuthenticationFilter(manager: ReactiveAuthenticationManager): AuthenticationWebFilter {
        val bearerAuthenticationFilter = AuthenticationWebFilter(manager)

        bearerAuthenticationFilter.setServerAuthenticationConverter { exchange ->
            exchange
                .request
                .headers
                .getOrDefault(HttpHeaders.AUTHORIZATION, emptyList())
                .firstOrNone()
                .filter { it.startsWith("Bearer ") }
                .map { it.substring(7) }
                .toEither { KJWTVerificationError.InvalidJWT }
                .flatMap { verifySignature<JWSHMAC256Algorithm>(it, JWT_SECRET) }
                .flatMap { it.claimValue("role").toEither { KJWTVerificationError.InvalidJWT } }
                .fold(
                    ifLeft = { Mono.empty() },
                    ifRight = {
                        UsernamePasswordAuthenticationToken(
                            null,
                            null,
                            listOf(SimpleGrantedAuthority("ROLE_$it"))
                        ).toMono()
                    },
                )
        }

        return bearerAuthenticationFilter
    }

    companion object {
        const val JWT_SECRET = "secret"
    }
}
