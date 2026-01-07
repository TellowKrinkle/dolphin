#!/bin/bash

set -e

export MACOSX_DEPLOYMENT_TARGET=11.0
INSTALLDIR="$HOME/deps"
NPROCS="$(getconf _NPROCESSORS_ONLN)"
SDL=SDL3-3.4.0
QT=6.5.8
QT_SUFFIX=-opensource

mkdir deps-build
cd deps-build

export PKG_CONFIG_PATH="$INSTALLDIR/lib/pkgconfig:$PKG_CONFIG_PATH"
export LDFLAGS="-L$INSTALLDIR/lib -dead_strip $LDFLAGS"
export CFLAGS="-I$INSTALLDIR/include -Os $CFLAGS"
export CXXFLAGS="-I$INSTALLDIR/include -Os $CXXFLAGS"

cat > SHASUMS <<EOF
082cbf5f429e0d80820f68dc2b507a94d4cc1b4e70817b119bbb8ec6a69584b8  $SDL.tar.gz
5d35f8f37dfb3dff387cce31725b3a363894e3b6ea9d86aa54d9e7fd34e1b888  qtbase-everywhere$QT_SUFFIX-src-$QT.tar.xz
7bebe5c5b63a6107027beecfa4dc1d8dd4837527040529a4c9d4159844c26061  qtsvg-everywhere$QT_SUFFIX-src-$QT.tar.xz
1c99dd8d3fd4044866a2d3d569ccd08e25ba28261d494f58b464fcfc18b719a8  qttools-everywhere$QT_SUFFIX-src-$QT.tar.xz
a9d50b947a4e6eb5c03c9b6b6b2cb4ca9a5db7064efe4bb2957976fe286a4dc0  qttranslations-everywhere$QT_SUFFIX-src-$QT.tar.xz
EOF

curl -L \
	-O "https://libsdl.org/release/$SDL.tar.gz" \
	-O "https://download.qt.io/archive/qt/${QT%.*}/$QT/src/submodules/qtbase-everywhere$QT_SUFFIX-src-$QT.tar.xz" \
	-O "https://download.qt.io/archive/qt/${QT%.*}/$QT/src/submodules/qtsvg-everywhere$QT_SUFFIX-src-$QT.tar.xz" \
	-O "https://download.qt.io/archive/qt/${QT%.*}/$QT/src/submodules/qttools-everywhere$QT_SUFFIX-src-$QT.tar.xz" \
	-O "https://download.qt.io/archive/qt/${QT%.*}/$QT/src/submodules/qttranslations-everywhere$QT_SUFFIX-src-$QT.tar.xz" \

shasum -a 256 --check SHASUMS

echo "Installing SDL..."
tar xf "$SDL.tar.gz"
cd "$SDL"
cmake -B build -DCMAKE_OSX_ARCHITECTURES="x86_64;arm64" -DCMAKE_PREFIX_PATH="$INSTALLDIR" -DCMAKE_INSTALL_PREFIX="$INSTALLDIR" -DCMAKE_BUILD_TYPE=MinSizeRel -DSDL_X11=OFF
make -C build "-j$NPROCS"
make -C build install
cd ..

echo "Installing Qt Base..."
tar xf "qtbase-everywhere$QT_SUFFIX-src-$QT.tar.xz"
cd "qtbase-everywhere-src-$QT"
cmake -B build -DCMAKE_OSX_ARCHITECTURES="x86_64;arm64" -DCMAKE_PREFIX_PATH="$INSTALLDIR" -DCMAKE_INSTALL_PREFIX="$INSTALLDIR" -DCMAKE_BUILD_TYPE=Release -DFEATURE_optimize_size=ON -DFEATURE_dbus=OFF -DFEATURE_framework=OFF -DFEATURE_icu=OFF -DFEATURE_opengl=OFF -DFEATURE_printsupport=OFF -DFEATURE_sql=OFF -DFEATURE_gssapi=OFF -DFEATURE_system_png=OFF -DFEATURE_system_jpeg=OFF -DCMAKE_MESSAGE_LOG_LEVEL=STATUS
make -C build "-j$NPROCS"
make -C build install
cd ..

echo "Installing Qt SVG..."
tar xf "qtsvg-everywhere$QT_SUFFIX-src-$QT.tar.xz"
cd "qtsvg-everywhere-src-$QT"
cmake -B build -DCMAKE_PREFIX_PATH="$INSTALLDIR" -DCMAKE_INSTALL_PREFIX="$INSTALLDIR" -DCMAKE_BUILD_TYPE=Release
make -C build "-j$NPROCS"
make -C build install
cd ..

echo "Installing Qt Tools..."
tar xf "qttools-everywhere$QT_SUFFIX-src-$QT.tar.xz"
cd "qttools-everywhere-src-$QT"
# Linguist relies on a library in the Designer target, which takes 5-7 minutes to build on the CI
# Avoid it by not building Linguist, since we only need the tools that come with it
patch -u src/linguist/CMakeLists.txt <<EOF
--- src/linguist/CMakeLists.txt
+++ src/linguist/CMakeLists.txt
@@ -14,7 +14,7 @@
 add_subdirectory(lrelease-pro)
 add_subdirectory(lupdate)
 add_subdirectory(lupdate-pro)
-if(QT_FEATURE_process AND QT_FEATURE_pushbutton AND QT_FEATURE_toolbutton AND TARGET Qt::Widgets AND NOT no-png)
+if(QT_FEATURE_process AND QT_FEATURE_pushbutton AND QT_FEATURE_toolbutton AND TARGET Qt::Widgets AND TARGET Qt::PrintSupport AND NOT no-png)
     add_subdirectory(linguist)
 endif()
EOF
cmake -B build -DCMAKE_PREFIX_PATH="$INSTALLDIR" -DCMAKE_INSTALL_PREFIX="$INSTALLDIR" -DCMAKE_BUILD_TYPE=Release -DFEATURE_assistant=OFF -DFEATURE_clang=OFF -DFEATURE_designer=OFF -DFEATURE_kmap2qmap=OFF -DFEATURE_pixeltool=OFF -DFEATURE_pkg_config=OFF -DFEATURE_qev=OFF -DFEATURE_qtattributionsscanner=OFF -DFEATURE_qtdiag=OFF -DFEATURE_qtplugininfo=OFF -DFEATURE_linguist=ON
make -C build "-j$NPROCS"
make -C build install
cd ..

echo "Installing Qt Translations..."
tar xf "qttranslations-everywhere$QT_SUFFIX-src-$QT.tar.xz"
cd "qttranslations-everywhere-src-$QT"
cmake -B build -DCMAKE_PREFIX_PATH="$INSTALLDIR" -DCMAKE_INSTALL_PREFIX="$INSTALLDIR" -DCMAKE_BUILD_TYPE=Release
make -C build "-j$NPROCS"
make -C build install
cd ..

echo "Cleaning up..."
cd ..
rm -r deps-build
