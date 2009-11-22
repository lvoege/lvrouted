#!/bin/sh
# Small tool to build lvrouted release, ready for distribution:
# * Documenation pregenerated (as docbook-xsl, ghostscript is required)
# * Subversion Version.ml file outfiled

# Version target
VERSION_ML="./src/Version.ml"
VERSION=`awk -F= '/let version/ {print $2}' $VERSION_ML`

TMPDIR=`mktemp -d -t lvrouted`

PKGDIR=lvrouted-$VERSION
WRKSRC=$TMPDIR/$PKGDIR
mkdir $WRKSRC
cp -R . $WRKSRC

# Fixed version storage
grep -e '^let version' -e '^let branch' $VERSION_ML > $WRKSRC/VERSION

# Do not build document files on release
sed 's/^all: lvrouted.html lvrouted.pdf/all:/g' $WRKSRC/Makefile.in > $WRKSRC/Makefile.in.new
mv $WRKSRC/Makefile.in.new $WRKSRC/Makefile.in

# Make pretty tar file out of it
tar --exclude ".svn" --exclude "Makefile" \
  --exclude "config.cache"  --exclude "config.log" --exclude "config.status" \
  --exclude "src/Version.ml" \
  --exclude "src/.depend" \
  --exclude "src/*.o" \
  --exclude "src/*.cmi" \
  --exclude "src/*.cmx" \
  --exclude ".*" \
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

rm -fR $TMPDIR
