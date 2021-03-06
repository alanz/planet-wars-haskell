#!/bin/sh

# Run a bot against the standard one

#export MY_BOT="./MyBot"
export MY_BOT="./dist/build/AzBot/AzBot"

#export OTHER_BOT=`"java -jar example_bots/RandomBot.jar"`
#export OTHER_BOT="./MyBot"
#export OTHER_BOT=$MY_BOT
export OTHER_BOT="./dist/build/MyBot/MyBot"

export MAP="maps/map7.txt"

java -jar tools/PlayGame.jar $MAP 1000 1000 log.txt \
        $MY_BOT $OTHER_BOT | \
        java -jar tools/ShowGame.jar

