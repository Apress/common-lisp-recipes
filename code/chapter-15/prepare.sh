#!/bin/sh
cd /tmp
rm -f foo* bar* quux*
echo "Waka/Jawaka" > foo.txt
ln -s foo.txt bar.txt
ln -s bar.txt quux.txt
