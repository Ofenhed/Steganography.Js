#EXPORTED_FUNCTIONS="['_cryptonite_aes_decrypt_cbc','_cryptonite_aes_decrypt_ecb','_cryptonite_aes_encrypt_cbc','_cryptonite_aes_encrypt_ctr','_cryptonite_aes_encrypt_ecb','_cryptonite_aes_gcm_aad','_cryptonite_aes_gcm_decrypt','_cryptonite_aes_gcm_encrypt','_cryptonite_aes_gcm_finish','_cryptonite_aes_gcm_init','_cryptonite_aes_initkey','_cryptonite_aes_ocb_aad','_cryptonite_aes_ocb_decrypt','_cryptonite_aes_ocb_encrypt','_cryptonite_aes_ocb_finish','_cryptonite_aes_ocb_init','_cryptonite_blake2b_finalize','_cryptonite_blake2b_init','_cryptonite_blake2b_update','_cryptonite_cpu_has_rdrand','_cryptonite_curve25519_donna','_cryptonite_ed25519_publickey','_cryptonite_ed25519_sign','_cryptonite_ed25519_sign_open','_cryptonite_get_rand_bytes','_cryptonite_sha1_finalize','_cryptonite_sha1_init','_cryptonite_sha1_update','_cryptonite_sha3_finalize','_cryptonite_sha3_init','_cryptonite_sha3_update','_cryptonite_sha512_finalize','_cryptonite_sha512_init','_cryptonite_sha512_update','_cryptonite_skein512_finalize','_cryptonite_skein512_init','_cryptonite_skein512_update']"
EXPORTED_FUNCTIONS=""
#for FUNC in $(cat cbits/exported.txt && cat ../zlib-0.6.1.2/exported.txt) ; do
for FUNC in $(cat cbits/exported.txt) ; do
  if [ "$EXPORTED_FUNCTIONS" == "" ] ; then
    EXPORTED_FUNCTIONS="["
  else
    EXPORTED_FUNCTIONS+=","
  fi
  EXPORTED_FUNCTIONS+="'"'_'$FUNC"'"
done
EXPORTED_FUNCTIONS+="]"

FLAGS="-s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS -msse3 -O2 -msse -msse2 -msse3 -s SIMD=1 -s ASSERTIONS=1 -s BINARYEN_METHOD='native-wasm,asmjs' -s TOTAL_MEMORY=1006632960"
CFLAGS="$FLAGS -DWITH_ASSERT_ALIGNMENT -DARCH_X86 -DSUPPORT_SSE -DWITH_PCLMUL" #" # -DWITH_AESNI" # 
LDFLAGS="$FLAGS -s STACK_OVERFLOW_CHECK=1"

echo ${EXPORTED_FUNCTIONS}

CC="emcc -c -I cbits/ $CFLAGS"
$CC cbits/ed25519/*.c cbits/cryptonite_sha512.c && \
$CC cbits/aes/x86ni.c cbits/cryptonite_aes.c -maes -mpclmul && \
$CC cbits/aes/{generic,gf}.c cbits/cryptonite_aes.c && \
$CC cbits/cryptonite_skein512.c && \
$CC -I cbits/blake2/ref cbits/blake2/ref/*.c cbits/cryptonite_blake2b.c && \
$CC cbits/curve25519/curve25519-donna.c && \
$CC cbits/cryptonite_sha1.c && \
$CC cbits/cryptonite_sha3.c && \
emcc $LDFLAGS -o cryptonite.js gf.o generic.o cryptonite_aes.o cryptonite_sha3.o cryptonite_sha1.o curve25519-donna.o cryptonite_blake2b.o blake2b-ref.o cryptonite_sha512.o cryptonite_skein512.o ed25519.o
echo Exported $EXPORTED_FUNCTIONS
