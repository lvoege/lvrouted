#!/bin/sh
# sample /usr/local/etc/rc.d/lvrouted.sh script to start the thing when
# booting

pid=`ps ax | grep lvrouted.opt | grep -v grep | sed "s/^ *//" | sed "s/ .*//g"`
case "$1" in
start)
	if test "$pid" != ""; then
		echo "Already running" >&2
	else
		/sbin/sysctl net.inet.ip.forwarding=1
		/usr/local/sbin/lvrouted.opt -u
	fi
	;;
reload)
	if test "$pid" != ""; then
		kill -HUP $pid
	else
		echo "Not running, will start now"
		/usr/local/sbin/lvrouted.opt -u
	fi
	;;
restart)
	if test "$pid" != ""; then
		kill -9 $pid
		route flush; route flush; route flush; route flush; route flush
		/usr/local/sbin/lvrouted.opt -u
	else
		echo "Not running, will start now"
		/usr/local/sbin/lvrouted.opt -u
	fi
	;;
stop)
	if test "$pid" != ""; then
		kill $pid
	else
		echo "Not running, nothing to kill"
	fi
	;;
*)
	echo "Usage: `basename $0` {start|stop|reload|restart}" >&2
	;;
esac
