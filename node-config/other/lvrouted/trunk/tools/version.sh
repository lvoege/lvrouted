#!/bin/sh

echo \(\* DO NOT EDIT BY HAND \*\) > Version.ml
if [ -e ".svn/entries" ]; then
	echo Extracting svn version info
	echo let version=`svn info . | grep Revision | sed "s/.* //g"` >> Version.ml
	echo let branch=\"`pwd | sed "s/.*lvrouted\///g" | sed "s/\/src//g"`\" >> Version.ml
else
	echo Extracting svk version info
	echo let version=`svk info . | grep Mirrored | sed "s/.*Rev. //g"` >> Version.ml
	echo let branch=\"`svk info . | grep Depot | sed "s/.*lvrouted\///g" | sed "s/\/src//g"`\" >> Version.ml
		
fi
echo let date=\"`date`\" >> Version.ml
echo let host=\"`uname -a`\" >> Version.ml
echo let ocamlopt=\"`ocamlopt -v | head -1`\" >> Version.ml
echo let who=\"`whoami`\" >> Version.ml
