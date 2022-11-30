#!/bin/bash

set -e

export MACOSX_DEPLOYMENT_TARGET=10.13
INSTALLDIR="$HOME/deps"
NPROCS="$(getconf _NPROCESSORS_ONLN)"
SDL=SDL2-2.26.0
QT=5.15.7

mkdir deps-build
cd deps-build

export PKG_CONFIG_PATH="$INSTALLDIR/lib/pkgconfig:$PKG_CONFIG_PATH"
export LDFLAGS="-L$INSTALLDIR/lib -dead_strip $LDFLAGS"
export CFLAGS="-I$INSTALLDIR/include -Os $CFLAGS"
export CXXFLAGS="-I$INSTALLDIR/include -Os $CXXFLAGS"

cat > SHASUMS <<EOF
8000d7169febce93c84b6bdf376631f8179132fd69f7015d4dadb8b9c2bdb295  $SDL.tar.gz
05edd00b2a1ba99c85b3fe876fe1c23d63f0a9bbca7df52bc47393cfd8c809c7  qtbase-everywhere-opensource-src-$QT.tar.xz
EOF

curl -L \
	-O "https://libsdl.org/release/$SDL.tar.gz" \
	-O "https://download.qt.io/official_releases/qt/${QT%.*}/$QT/submodules/qtbase-everywhere-opensource-src-$QT.tar.xz" \

shasum -a 256 --check SHASUMS

echo "Installing SDL..."
tar xf "$SDL.tar.gz"
cd "$SDL"
./configure --prefix "$INSTALLDIR" --without-x
make "-j$NPROCS"
make install
cd ..

echo "Installing Qt Base..."
tar xf "qtbase-everywhere-opensource-src-$QT.tar.xz"
cd "qtbase-everywhere-src-$QT"
patch -u src/plugins/platforms/cocoa/qiosurfacegraphicsbuffer.h <<EOF
--- src/plugins/platforms/cocoa/qiosurfacegraphicsbuffer.h
+++ src/plugins/platforms/cocoa/qiosurfacegraphicsbuffer.h
@@ -44,4 +44,5 @@
 #include <private/qcore_mac_p.h>
+#include <CoreGraphics/CGColorSpace.h>
 
 QT_BEGIN_NAMESPACE
EOF
./configure -prefix "$INSTALLDIR" -release -optimize-size -no-framework -no-feature-sql -no-feature-xml -no-feature-testlib -no-feature-network -no-feature-dbus -no-feature-concurrent -opensource -confirm-license
make "-j$NPROCS"
make install
cd ..

echo "Cleaning up..."
cd ..
rm -r deps-build
