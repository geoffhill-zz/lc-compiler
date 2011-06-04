#!/bin/bash

DIR=`dirname $0`
if [ `uname` == "Darwin" ]
  then
    RACKET_PATH="/Applications/Racket v5.1/bin/racket"
    AS="as -arch i386"
    CC="gcc -arch i386 -O0"
  else
    RACKET_PATH="/opt/racket-5.1/bin/racket"
    AS="as"
    CC="gcc4 -O0"
fi

FPREFIX=`echo $RANDOM`
EXEC="a.out"

if [ -z "$1" ];
  then
    echo "Usage: graph-test PROGRAM"
  else
    if [ "$ASSEMBLE" == "1" ]
      then
        "$RACKET_PATH" -tm "$DIR/driver.rkt" "$COMMAND" "$1" > "$FPREFIX.asm"
        # TODO: fix GCC optimization
        $AS -o "$FPREFIX.o" "$FPREFIX.asm"
        $CC -o "$EXEC" "$FPREFIX.o" "L1-runtime.c"
        chmod +x "$EXEC"
        rm -f "$FPREFIX.asm" "$FPREFIX.o"
      else
        "$RACKET_PATH" -tm "$DIR/driver.rkt" "$COMMAND" "$1"
    fi
fi

