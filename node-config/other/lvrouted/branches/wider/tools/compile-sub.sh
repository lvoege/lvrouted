#!/bin/sh
# this runs inside some jail, to compile an lvrouted.opt binary
cd /root/lvrouted
./configure
cd src
touch *.ml
make
mv lvrouted.opt lvrouted.opt-`hostname`
