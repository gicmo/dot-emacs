#!/bin/sh

ironypkg=(elpa/irony-*)
ironysrc="${ironypkg}/server"
builddir=irony/build
target=irony
binary=irony/bin/irony-server

echo "server source: ${ironysrc}"

if [ ! -e irony/include ]; then
    echo "downloading clang headers [svn]"
    svn export http://llvm.org/svn/llvm-project/cfe/trunk/include/clang-c/ irony/include/clang-c
fi

rm -rf "$builddir" && mkdir -p "$builddir"

libclang=`xcode-select --print-path`/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib

echo "libclang @ $libclang"

cmake -DCMAKE_INSTALL_PREFIX="$target" \
      -DLIBCLANG_INCLUDE_DIR="irony/include" \
      -DLIBCLANG_LIBRARY="$libclang" \
      -H"$ironysrc" -B"$builddir" \
    && cmake --build "$builddir" --use-stderr --config Release --target install && \
    install_name_tool -change @rpath/libclang.dylib "$libclang" "$binary" && \
    "$binary" -v

