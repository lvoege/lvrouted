#!/bin/sh
# tooltje om de daemon op nodes te updaten. gebruik:
#   ./update.sh cetim1 lvrouted.opt /usr/local/sbin/lvrouted.opt "/usr/local/etc/rc.d/lvrouted.sh restart"
# of 
#   ./update.sh cetim1 lvrouted.sh /usr/local/etc/rc.d/lvrouted.sh ""
#
# m.a.w., nodenaam, lokaal pad, remote pad, commando om uit te voeren.
# mv't eerst de oude binary weg om het "Text file is busy" te omzeilen,
# kopieert dan de gegeven file, maakt 'em executable, remount root readonly
# als het een soekris is, en voert het commando uit.
node=$1
from=$2
to=$3
cmd=$4
cat $from | ssh -C root@$node.wleiden.net \
	"mount -u -o noatime -w /; \
	mv -f $to $to.old; \
	cat > $to; \
	chmod +x $to; \
	uname -a | grep -q SOEKRIS && mount -u -r / ; \
	$cmd"
