#!/bin/bash

# on raspbian, build the program and reboot to it

set -ex
for CHIP in 2837 # 2835 2836 2837
do
    ./build.sh $CHIP
    sudo cp ultibo-ble-peripheral-kernel-$CHIP.img /boot/
done

sudo cp *.hcd /boot
sudo cp ultibo-ble-peripheral-config.txt ultibo-ble-peripheral-cmdline.txt /boot
sudo cp /boot/ultibo-ble-peripheral-config.txt /boot/config.txt
sudo reboot
