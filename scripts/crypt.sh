#!/bin/bash

if [ -e ".rivetcrypt" ]
then
    openssl enc -aes-256-cbc -d -a -salt -in .rivetcrypt -out /tmp/rivetdecrypted -pass file:.rivetpass
else
    touch /tmp/rivetdecrypted
fi
$EDITOR /tmp/rivetdecrypted
openssl enc -aes-256-cbc -e -a -salt -in /tmp/rivetdecrypted -out .rivetcrypt -pass file:.rivetpass
rm /tmp/rivetdecrypted
