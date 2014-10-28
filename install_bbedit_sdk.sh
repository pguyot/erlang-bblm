#!/bin/sh
if [ ! -e "BBEdit SDK" ]; then
    curl -s http://www.barebones.com/support/develop/BBEditSDK.dmg -o BBEditSDK.dmg
    mkdir -p mnt
    hdiutil mount BBEditSDK.dmg -mountpoint mnt
    cp -R "mnt" "BBEdit SDK"
    hdiutil unmount "mnt"
    rm BBEditSDK.dmg
    rmdir mnt
fi
