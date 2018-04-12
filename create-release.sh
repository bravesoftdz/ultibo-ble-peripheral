#!/bin/bash
set -e # exit script on any error

VERSION=v$(date +%Y.%m.%d.%H%M)
PREFIX=ultibo-ble-peripheral
ZIPFILE=$PREFIX-$VERSION.zip
PATH=$HOME/hub-linux-arm-2.3.0-pre10/bin:$PATH

mkdir -p release
rm -rf release/*

rm -f *kernel*.img
for CHIP in 2835 2836 2837
do
    ./build.sh $CHIP
done
cp -a *.hcd release/
cp -a *.img release/
cp -a $PREFIX-config.txt $PREFIX-cmdline.txt release/
cp -a release/$PREFIX-config.txt release/config.txt
echo "$PREFIX $VERSION" >> release/release-message.md
echo >> release/release-message.md
cat release-message.md >> release/release-message.md
cp -a firmware/boot/bootcode.bin firmware/boot/start.elf firmware/boot/fixup.dat release/
cd release
zip $ZIPFILE *
cd ..

hub release create -d -p -F release/release-message.md -a release/$ZIPFILE $VERSION
