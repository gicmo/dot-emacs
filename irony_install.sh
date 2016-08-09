#!/bin/sh

ironypkg=(elpa/irony-*)
ironysrc="${ironypkg}/server"
builddir=irony/build
target=irony
binary=irony/bin/irony-server
system=`uname`

echo "system: ${system}"
echo "server source: ${ironysrc}"

if [[ "${system}" == "Darwin" ]]; then
    if [ ! -e irony/include ]; then
	echo "downloading clang headers [svn]"
	svn export http://llvm.org/svn/llvm-project/cfe/trunk/include/clang-c/ irony/include/clang-c
    fi
    CLANG_INCDIR="irony/include"
    CLANG_LIB=`xcode-select --print-path`/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib
else
    CLANG_INCDIR="/usr/include/clang-c"
    CLANG_LIB="/usr/lib64/libclang.so"
fi

echo "clang hdr @ ${CLANG_INCDIR}"
echo "clang lib @ ${CLANG_LIB}"

rm -rf "$builddir" && mkdir -p "$builddir"

cmake -DCMAKE_INSTALL_PREFIX="$target" \
      -DLIBCLANG_INCLUDE_DIR="${CLANG_INCDIR}" \
      -DLIBCLANG_LIBRARY="${CLANG_LIB}" \
      -H"$ironysrc" -B"$builddir" \
    && cmake --build "$builddir" --use-stderr --config Release --target install

if [[ $? -ne 0 ]]; then
    echo "ERROR building irony server :("
    exit $?;
fi


if [[ "${system}" == "Darwin" ]]; then
    install_name_tool -change @rpath/libclang.dylib "$CLANG_LIB" "$binary"
fi

"$binary" -v

