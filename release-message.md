# Ultibo Bluetooth Low Energy (BLE) Peripheral Test

# Requires:
* a computer that can write an sd card
* an sd card that you can erase - its contents will be destroyed
* an RPI3B or RPI3B+ with a power supply
* an hdmi tv and an hdmi cable

# Optional:
* usb keyboard

# Steps:
* with the computer
    * download the zip file
    * format the sd card as FAT32
        * this destroys the current contents of the sd card
    * unzip it to the sd card
* insert the sd card into the pi
* connect the pi to the tv using the hdmi cable
* connect the optional usb keyboard to the pi
* turn on the tv
* apply power to the pi
* you should see a green border with large white regions with black text

# Operation:
* scan for device "Ultibo" using nRF Connect (by Nordic) on the phone
* the name attribute should return "Ultibo Name"
* the immediate alert attribute when written should show a message in the log
* the battery level can be read - use +/- on the usb keyboard to change it
