#!/bin/sh
# Small tool to build lvrouted release, ready for distribution:
# * Documenation pregenerated (as docbook-xsl, ghostscript is required)
# * Subversion Version.ml file outfiled

# Version target
VERSION=`awk -F= '/let version/ {print $2}' src/Version.ml`

TMPDIR=`mktemp -d -t lvrouted`

PKGDIR=lvrouted-$VERSION
WRKSRC=$TMPDIR/$PKGDIR
mkdir $WRKSRC
cp -R . $WRKSRC

# Fixed version storage
grep -e '^let version' -e '^let branch' src/Version.ml > $WRKSRC/VERSION

# Do not build document files on release
sed 's/^all: lvrouted.html lvrouted.pdf/all:/g' $WRKSRC/Makefile.in > $WRKSRC/Makefile.in.new
mv $WRKSRC/Makefile.in.new $WRKSRC/Makefile.in

# Make pretty tar file out of it
tar --exclude ".svn" --exclude "Makefile" \
  --exclude "config.cache"  --exclude "config.log" --exclude "config.status" \
  --exclude ".depend" \
  -cvzf lvrouted-$VERSION.tar.gz \
  -C $TMPDIR \
  $PKGDIR/INSTALL \
  $PKGDIR/LICENSE \
  $PKGDIR/Makefile.in \
  $PKGDIR/README \
  $PKGDIR/TODO \
  $PKGDIR/configure.ac \
  $PKGDIR/docs \
  $PKGDIR/src \
  $PKGDIR/tools  \
  $PKGDIR/VERSION

rm -R $TMPDIR
