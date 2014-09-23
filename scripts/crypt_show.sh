#!/bin/bash

if [ -e ".rivetcrypt" ]
then
    openssl enc -aes-256-cbc -d -a -salt -in .rivetcrypt -pass file:.rivetpass
else
    echo "No .rivetcrypt."
fi
