#!/bin/sh
# We unfortunately need to support a whole slew of FreeBSD versions. this
# script fires off compiles for every one of 'em, given the jail configuration
# on my machine. It therefore serves mainly as an example to skim off of
# rather than use directly.
jail /usr/jails/5.0 5.0 192.168.123.2 /root/lvrouted/tools/compile-sub.sh
jail /usr/jails/5.4 5.4 192.168.123.3 /root/lvrouted/tools/compile-sub.sh
jail /usr/jails/6.0 6.0 192.168.123.4 /root/lvrouted/tools/compile-sub.sh
jail /usr/jails/6.1 6.1 192.168.123.5 /root/lvrouted/tools/compile-sub.sh
jail /usr/jails/6.2 6.2 192.168.123.6 /root/lvrouted/tools/compile-sub.sh
