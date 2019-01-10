#!/bin/bash
cd ..
set -o errexit
rm -rf namedCapture-release
cp -r namedCapture namedCapture-release
PKG_TGZ=$(R CMD build namedCapture-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R --vanilla CMD INSTALL $PKG_TGZ
R --vanilla CMD check --as-cran $PKG_TGZ
