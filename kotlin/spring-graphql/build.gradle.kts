import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

@Suppress("DSL_SCOPE_VIOLATION")
plugins {
    alias(libs.plugins.spring.boot)
    alias(libs.plugins.spring.dependency.management)
    alias(libs.plugins.ktlint)
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
}

group = "com.graphql"
version = "0.0.1-SNAPSHOT"
java.sourceCompatibility = JavaVersion.VERSION_17

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.spring.data.mongodb.reactive)
    implementation(libs.spring.graphql)
    implementation(libs.spring.webflux)
    implementation(libs.spring.security)
    implementation(libs.bundles.kotlin.libs)
    implementation(libs.arrow.core)
    implementation(libs.kjwt.core)
    implementation(libs.mockk)
    implementation(libs.spring.mockk)
    testImplementation(libs.spring.starter.test) {
        exclude(module = "mockito-core")
    }
    testImplementation(libs.spring.security.test)
    testImplementation(libs.spring.graphql.test)
    testImplementation(libs.embed.mongo)
    testImplementation(libs.reactore.test)
    testImplementation(libs.bundles.kotest)
}

tasks.withType<KotlinCompile> {
    kotlinOptions {
        freeCompilerArgs = listOf("-Xjsr305=strict")
        jvmTarget = "17"
    }
}

tasks.withType<Test> {
    useJUnitPlatform()
}
