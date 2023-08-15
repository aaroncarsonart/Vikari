#!/bin/zsh

# Build project with `mvn install` before running script.
java -jar `ls -r target/Vikari* | head -n1` "$@"
