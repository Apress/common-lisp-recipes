#!/bin/sh
cd /tmp
rm -rf foo quux
mkdir -p foo/bar
ln -s foo/bar quux
