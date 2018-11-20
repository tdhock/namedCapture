#!/bin/bash
cd ..
set -o errexit
rm -rf namedCapture-release
cp -r namedCapture namedCapture-release
PKG_TGZ=$(R CMD build namedCapture-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
