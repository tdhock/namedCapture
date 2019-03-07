#!/bin/bash
cd ..
set -o errexit
rm -rf namedCapture-release
cp -r namedCapture namedCapture-release
PKG_TGZ=$(R CMD build namedCapture-release|tee namedCapture-release-build.txt|grep building|sed "s/.*\(namedCapture.*.tar.gz\).*/\1/")
cat namedCapture-release-build.txt
R --vanilla CMD INSTALL $PKG_TGZ
R --vanilla CMD check --as-cran $PKG_TGZ
