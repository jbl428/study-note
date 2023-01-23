import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    id("org.springframework.boot") version "3.0.1" apply false
    id("io.spring.dependency-management") version "1.1.0" apply false
    kotlin("jvm") version "1.7.22"
    kotlin("plugin.spring") version "1.7.22" apply false
    kotlin("plugin.jpa") version "1.7.22" apply false
}

java.sourceCompatibility = JavaVersion.VERSION_17

allprojects {
    group = "com.example"
    version = "0.0.1"
    repositories { mavenCentral() }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = "org.springframework.boot")
    apply(plugin = "io.spring.dependency-management")
    apply(plugin = "kotlin-spring")

    dependencies {
        implementation("org.jetbrains.kotlin:kotlin-reflect")
        implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")

        testImplementation("org.springframework.boot:spring-boot-starter-test")
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
}
