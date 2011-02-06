#!/bin/sh
# tooltje om de daemon op nodes te updaten. gebruik:
#
#   ./update.sh [node] /pad/naar/install.sh tarfile 
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
node=$1
installsh=$2
tarball=$3
scp $tarball $installsh root@$node.wleiden.net:/tmp/
ssh -C root@$node.wleiden.net "sh /tmp/install.sh /tmp/$tarball"
