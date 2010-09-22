#!/bin/sh

# Make a file for submission

# Will fine-tune this in time

export CURRENT_BOT="./mybots/MyBot.hs"
cp $CURRENT_BOT MyBot.hs

rm submit.zip
zip submit.zip *.hs


