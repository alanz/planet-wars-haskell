#!/bin/sh

# Run a bot against the standard one

java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt \
        "./MyBot" "java -jar example_bots/RandomBot.jar" | \
        java -jar tools/ShowGame.jar

