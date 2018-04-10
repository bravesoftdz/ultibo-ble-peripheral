#!/bin/bash
set -ex

# on raspbian, build the program and reboot to it

CHIP=$1
if [[ $CHIP == "" ]]
then
    CHIP=2837
fi
case $CHIP in
2835)
    CONF=RPI
    PROC=RPIB
    ARCH=ARMV6
    KERNEL=kernel.img
    ;;
2836)
    CONF=RPI2
    PROC=RPI2B
    ARCH=ARMV7a
    KERNEL=kernel7.img
    ;;
2837)
    CONF=RPI3
    PROC=RPI3B
    ARCH=ARMV7a
    KERNEL=kernel7.img
    ;;
esac
BUILD_SYMBOL=BUILD_BCM$CHIP

ULTIBO=$HOME/ultibo/core
ULTIBOBIN=$ULTIBO/fpc/bin
LPR=BLETest.lpr
export PATH=$ULTIBOBIN:$PATH
for f in *.lpr *.pas

do
    ptop -l 1000 -i 1 -c ptop.cfg $f $f.formatted
    mv $f.formatted $f
done

rm -rf lib/ *.o
fpc -d$BUILD_SYMBOL -B -O2 -Tultibo -Parm -Cp$ARCH -Wp$PROC -Fi$ULTIBO/source/rtl/ultibo/extras -Fi$ULTIBO/source/rtl/ultibo/core @$ULTIBOBIN/$CONF.CFG $LPR >& errors.log

mv $KERNEL ultibo-ble-peripheral-kernel-$CHIP.img
