#!/bin/sh
# Given the jail configuration on my machine, mount a specified directory 
# under /root/lvrouted in all of them, plus mount /usr/ports
for v in `echo 5.0 5.4 6.0 6.1 6.2`; do
	umount /usr/jails/$i/root/lvrouted
	mount_nullfs $1 /usr/jails/$i/root/lvrouted
	mount_nullfs /usr/ports /usr/jails/$i/usr/ports
done
