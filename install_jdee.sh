#!/bin/sh

cd ~/.emacs.d

if [[ -d jdee-server ]]; then
    cd jdee-server
    git pull origin master
else
    git clone git@github.com:jdee-emacs/jdee-server.git
fi

mvn -DskipTests=true assembly:assembly
