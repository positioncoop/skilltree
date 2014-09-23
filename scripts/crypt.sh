#!/bin/bash

if [ -e ".rivetcrypt" ]
then
    gpg --yes -o /tmp/rivetdecrypted --passphrase-file .rivetpass -d .rivetcrypt
else
    touch /tmp/rivetdecrypted
fi
$EDITOR /tmp/rivetdecrypted
gpg --yes -o .rivetcrypt --symmetric --passphrase-file .rivetpass --cipher-algo AES256 < /tmp/rivetdecrypted
rm /tmp/rivetdecrypted
