#!/bin/sh

# Make a file for submission

# Will fine-tune this in time

export CURRENT_BOT="./mybots/AzBot.hs"


rm submit.zip
(cd v-olathe; cp $CURRENT_BOT MyBot.hs; zip ../submit.zip *.hs)


