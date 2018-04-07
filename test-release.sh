#!/bin/bash
set -x

mkdir -p testing
rm -rf testing/*
cp release/*.zip testing
pushd testing
unzip *.zip
rm *.zip
sudo cp ultibo-ble-peripheral-* config.txt /boot
sleep 5
sudo reboot
