#!/bin/zsh

# trim off "/src/main/sh/dnr.sh" from path
PROJECT_ROOT_DIRECTORY=${0:a:h:h:h:h}

# -----------
# DEBUG MODE:
# -----------
#java -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 -jar $PROJECT_ROOT_DIRECTORY/target/Vikari-1.0-SNAPSHOT.jar $1

# --------------
# MAIN MODE:
# --------------
java -jar $PROJECT_ROOT_DIRECTORY/target/Vikari-1.0-SNAPSHOT.jar $1
