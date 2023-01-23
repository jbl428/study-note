dependencies {
    implementation(project(":jpa-entity"))
    implementation("org.springframework.boot:spring-boot-starter-data-jpa")
    implementation("org.springframework.boot:spring-boot-starter-actuator")
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation("com.linecorp.kotlin-jdsl:spring-data-kotlin-jdsl-starter-jakarta:2.1.0.RELEASE")
}
