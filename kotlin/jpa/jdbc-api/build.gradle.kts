dependencies {
    implementation(project(":jdbc-entity"))
    implementation("org.springframework.boot:spring-boot-starter-data-jdbc")
    implementation("org.springframework.boot:spring-boot-starter-actuator")
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")

    testImplementation("org.flywaydb:flyway-core")
    testImplementation("io.kotest:kotest-assertions-core:2.4.1")
}
