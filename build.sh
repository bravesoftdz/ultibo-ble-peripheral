#!/bin/bash

# on raspbian, build the program and reboot to it

set -ex
ULTIBO=$HOME/ultibo/core
ULTIBOBIN=$ULTIBO/fpc/bin
LPR=BLETest.lpr
export PATH=$ULTIBOBIN:$PATH
for f in *.lpr *.pas

do
    ptop -l 1000 -i 1 -c ptop.cfg $f $f.formatted
    mv $f.formatted $f
done

rm -rf lib/
fpc -B -O2 -Tultibo -Parm -CpARMV7a -WpRPI3B -Fi$ULTIBO/source/rtl/ultibo/core @$ULTIBOBIN/RPI3.CFG $LPR >& errors.log
