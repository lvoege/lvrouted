#!/bin/sh
# sample /usr/local/etc/rc.d/lvrouted.sh script to start the thing when
# booting

pid=`ps ax | grep lvrouted.opt | grep -v grep | sed "s/ .*//g"`
case "$1" in
start)
	if test "$pid" != ""; then
		echo "Already running" >&2
	else
		/sbin/sysctl net.inet.ip.forwarding=1
		/usr/local/sbin/lvrouted.opt -u
	fi
	;;
restart)
	if test "$pid" != ""; then
		kill -HUP $pid
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
	echo "Usage: `basename $0` {start|stop|restart}" >&2
	;;
esac
