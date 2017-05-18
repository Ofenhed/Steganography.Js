#!/bin/bash

EXPORT_FILE=cbits/exported.txt

if [ ! -f "$EXPORT_FILE" ] || [ $# -ne 0 ] ; then
  if [ $# -eq 0 ] ; then
    echo "Usage: $0 all.js"
    echo "The all.js is the file compiled by ghcjs"
    exit 1
  fi
  find cbits/ -name \*.[ch] -exec ctags --c-types=fp {} \+
  ALL_FUNCTIONS=$(awk '{ print $1 }' tags | sort -u)
  echo -n > $EXPORT_FILE
  for FUNCTION in $ALL_FUNCTIONS ; do
    if grep "h\$$FUNCTION(" $1 > /dev/null ; then
      echo "$FUNCTION" >> "$EXPORT_FILE"
    fi
  done
  echo "$EXPORT_FILE created, manually verify that all functions are correct before continuing."
  exit 0
fi

EXPORTED_FUNCTIONS=""

for FUNC in $(cat $EXPORT_FILE) ; do
  if [ "$EXPORTED_FUNCTIONS" == "" ] ; then
    EXPORTED_FUNCTIONS="["
  else
    EXPORTED_FUNCTIONS+=","
  fi
  EXPORTED_FUNCTIONS+="'"'_'$FUNC"'"
done
EXPORTED_FUNCTIONS+="]"

FLAGS="-s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS -msse3 -O2 -msse -msse2 -msse3 -s ASSERTIONS=1 -s TOTAL_MEMORY=1006632960" #-maes -mpclmul -s SIMD=1
CFLAGS="$FLAGS -DWITH_ASSERT_ALIGNMENT -DARCH_X86 -DSUPPORT_SSE" #-DWITH_PCLMUL" # -DWITH_AESNI"
LDFLAGS="$FLAGS -s STACK_OVERFLOW_CHECK=1 -s NO_EXIT_RUNTIME=1 -s BINARYEN_METHOD='native-wasm,asmjs' -s BINARYEN=1 -s MODULARIZE=1"

CC="emcc -c -I cbits/ $CFLAGS"
$CC cbits/ed25519/*.c cbits/cryptonite_sha512.c && \
$CC cbits/aes/x86ni.c cbits/cryptonite_aes.c && \
$CC cbits/aes/{generic,gf}.c cbits/cryptonite_aes.c && \
$CC cbits/cryptonite_skein512.c && \
$CC -I cbits/blake2/ref cbits/blake2/ref/*.c cbits/cryptonite_blake2b.c && \
$CC cbits/curve25519/curve25519-donna.c && \
$CC cbits/cryptonite_sha1.c && \
$CC cbits/cryptonite_sha3.c && \
emcc $LDFLAGS -o cryptonite.js gf.o x86ni.o generic.o cryptonite_aes.o cryptonite_sha3.o cryptonite_sha1.o curve25519-donna.o cryptonite_blake2b.o blake2b-ref.o cryptonite_sha512.o cryptonite_skein512.o ed25519.o
echo Exported $EXPORTED_FUNCTIONS
