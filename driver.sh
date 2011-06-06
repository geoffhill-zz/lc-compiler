#!/bin/bash

DIR=`dirname $0`
if [ `uname` == "Darwin" ]
  then
    RACKET_PATH="/Applications/Racket v5.1/bin/racket"
    AS="as -arch i386"
    CC="gcc -arch i386 -m32 -O0"
  else
    RACKET_PATH="/opt/racket-5.1/bin/racket"
    AS="as"
    CC="gcc4 -static -O2"
fi

FPREFIX=`echo $RANDOM`
EXEC="a.out"
ASMCPY="a.asm"

if [ -z "$1" ];
  then
    echo "Usage: MODE PROGRAM"
  else
    if [ "$ASSEMBLE" == "1" ]
      then
        rm -f "$EXEC" "$ASMCPY"
        "$RACKET_PATH" -tm "$DIR/driver.rkt" "$COMMAND" "$1" > "$FPREFIX.asm"
        cp "$FPREFIX.asm" "$ASMCPY"
        $AS -o "$FPREFIX.o" "$FPREFIX.asm"
        $CC -o "$EXEC" "L1-runtime.c" "$FPREFIX.o"
        chmod +x "$EXEC"
        rm -f "$FPREFIX.asm" "$FPREFIX.o"
      else
        "$RACKET_PATH" -tm "$DIR/driver.rkt" "$COMMAND" "$1"
    fi
fi

