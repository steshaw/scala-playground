#!/bin/sh

scalac -classpath /Users/steven/.m2/repository/org/slf4j/slf4j-api/1.5.8/slf4j-api-1.5.8.jar \
  *.scala

scala -cp .:/Users/steven/.m2//repository/org/slf4j/slf4j-jdk14/1.5.3/slf4j-jdk14-1.5.3.jar:\
/Users/steven/.m2/repository/org/slf4j/slf4j-api/1.5.8/slf4j-api-1.5.8.jar \
  Main
