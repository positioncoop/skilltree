#!/bin/bash

BACKUP=/backups

pg_dump skilltree_prod > $BACKUP/dump.txt

/usr/local/bin/tarsnap --cachedir /usr/tarsnap-cache --keyfile {{ base_dir }}/tarsnap.key -c -f skilltree-`date +%m.%d.%Y` $BACKUP && curl https://nosnch.in/2c7d6c71c4 &> /dev/null
