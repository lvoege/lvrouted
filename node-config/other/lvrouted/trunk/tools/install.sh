#!/bin/sh
# tooltje om de daemon op nodes te updaten. gebruik:
#
#   ./install.sh tarfile 
#
# met tarfile een tarfile met gecompilede binaries voor alle ondersteunde
# versies. dus bv:
# $ tar tvfz ../src/lvrouted-5707.tar.gz
# -rwxr-xr-x  0 root   lodewijk 470367 Nov 14 17:39 lvrouted.opt-5.0
# -rwxr-xr-x  0 root   lodewijk 421932 Nov 14 17:39 lvrouted.opt-5.4
# -rwxr-xr-x  0 root   lodewijk 417492 Nov 14 17:39 lvrouted.opt-6.0
# -rwxr-xr-x  0 root   lodewijk 417492 Nov 14 17:40 lvrouted.opt-6.1
# -rwxr-xr-x  0 root   lodewijk 421554 Nov 14 17:40 lvrouted.opt-6.2
#
tarball=$1
mount -u -o noatime -w /;
cd /usr/local/sbin
tar xfOz $tarball lvrouted.opt-`uname -r | sed "s/-.*//g"` > lvrouted.opt-new
rm $tarball
# als we hier zijn aangekomen zal het wel goed zitten
mv -f lvrouted.opt lvrouted.opt.old
mv lvrouted.opt-new lvrouted.opt
chmod +x lvrouted.opt
uname -a | grep -q SOEKRIS && mount -u -r /
/usr/local/etc/rc.d/lvrouted.sh restart
