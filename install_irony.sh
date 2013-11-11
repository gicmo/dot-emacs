#!/bin/sh

if [ ! -e irony-mode ]; then
    git clone --recursive git://github.com/Sarcasm/irony-mode.git
fi

cd irony-mode
mkdir -p build
cd build
cmake .. && make -j 4
make install
