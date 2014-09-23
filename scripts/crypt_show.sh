#!/bin/bash

if [ -e ".rivetcrypt" ]
then
    gpg -o - --passphrase-file .rivetpass -d .rivetcrypt
else
    echo "No .rivetcrypt."
fi
