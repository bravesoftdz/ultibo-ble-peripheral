#!/bin/bash

# on raspbian, build the program and reboot to it

set -ex
./build.sh
sudo cp BCM43430A1.hcd /boot
sudo cp kernel7.img /boot/ultibo-ble-peripheral-kernel7.img
sudo cp ultibo-ble-peripheral-config.txt ultibo-ble-peripheral-cmdline.txt /boot
sudo cp /boot/ultibo-ble-peripheral-config.txt /boot/config.txt
sudo reboot
