# kjson-core

[![Build Status](https://travis-ci.com/pwall567/kjson-core.svg?branch=main)](https://travis-ci.com/pwall567/kjson-core)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Kotlin](https://img.shields.io/static/v1?label=Kotlin&message=v1.5.20&color=7f52ff&logo=kotlin&logoColor=7f52ff)](https://github.com/JetBrains/kotlin/releases/tag/v1.5.20)
[![Maven Central](https://img.shields.io/maven-central/v/io.kjson/kjson-core?label=Maven%20Central)](https://search.maven.org/search?q=g:%22io.kjson%22%20AND%20a:%kjson-core%22)

JSON Kotlin core library

## Background

This library provides the basic functionality required to represent JSON values in Kotlin, including:
- parsing functions to convert JSON text to a structure of JSON values
- classes to hold the internal forms of the values
- output functions to create valid JSON representations

The library is an evolution of the [jsonutil](https://github.com/pwall567/jsonutil) Java library.

## Dependency Specification

The latest version of the library is 1.0, and it may be obtained from the Maven Central repository.

### Maven
```xml
    <dependency>
      <groupId>io.kjson</groupId>
      <artifactId>kjson-core</artifactId>
      <version>1.0</version>
    </dependency>
```
### Gradle
```groovy
    implementation "io.kjson:kjson-core:1.0"
```
### Gradle (kts)
```kotlin
    implementation("io.kjson:kjson-core:1.0")
```

Peter Wall

2021-08-02
