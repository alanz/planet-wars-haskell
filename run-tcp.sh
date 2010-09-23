#!/bin/sh

# Run a bot against the tcp server.
# See http://www.benzedrine.cx/planetwars/

export CFG=./$LOGNAME.cfg
export MY_BOT="./dist/build/AzBot/AzBot"

. $CFG
#echo $PASSWORD

#./tools/tcp 72.44.46.68 995 alan_zimm -p $PASSWORD $MY_BOT
java -jar tools/TCP.jar -d 72.44.46.68 995  alan_zimm -p $PASSWORD $MY_BOT


# PlanetWars tester TCP edition
#   USAGE:   java -jar tools/TCP.jar [-d] [address]   [port] [username]  [-p password] [bot command line]
#   EXAMPLE: java -jar tools/TCP.jar      72.44.46.68 995    javaexample               java MyBot

#   -d    turns on debug mode, which reveals what the server and bot send to each other
#   -p    transmits the password for the given username
