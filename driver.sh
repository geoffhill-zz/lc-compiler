#!/bin/bash

DIR=`dirname $0`

RACKET_PATH="racket"
if [ -x "/opt/racket-5.1/bin/racket" ]
then
  RACKET_PATH="/opt/racket-5.1/bin/racket"
fi
if [ -x "/Applications/Racket v5.1/bin/racket" ]
then
  RACKET_PATH="/Applications/Racket v5.1/bin/racket"
fi

if [ -z "$1" ];
  then
    echo "Usage: graph-test PROGRAM"
  else
    "$RACKET_PATH" -tm "$DIR/driver.rkt" "$COMMAND" "$1"
fi

