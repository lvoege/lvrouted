#!/bin/sh
echo \(\* DO NOT EDIT BY HAND \*\) > Version.ml
if [ -r '../VERSION' ]; then
	echo Extracting VERSION file info
	cat ../VERSION >> Version.ml
elif [ -e "../.svn/entries" -o -e "../.svn/wc.db" ]; then
	echo Extracting svn version info
	echo let version=`svn info . | grep Revision | sed "s/.* //g"` >> Version.ml
	echo let branch=\"`svn info . | grep URL | sed "s/.*lvrouted\///g" | sed "s/\/src//g"`\" >> Version.ml
fi
echo let date=\"`date`\" >> Version.ml
echo let host=\"`uname -a`\" >> Version.ml
echo let ocamlopt=\"`ocamlopt -v | head -1`\" >> Version.ml
echo let who=\"`whoami`\" >> Version.ml
