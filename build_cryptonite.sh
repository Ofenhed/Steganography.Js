#EXPORTED_FUNCTIONS="['_cryptonite_aes_decrypt_cbc','_cryptonite_aes_decrypt_ecb','_cryptonite_aes_encrypt_cbc','_cryptonite_aes_encrypt_ctr','_cryptonite_aes_encrypt_ecb','_cryptonite_aes_gcm_aad','_cryptonite_aes_gcm_decrypt','_cryptonite_aes_gcm_encrypt','_cryptonite_aes_gcm_finish','_cryptonite_aes_gcm_init','_cryptonite_aes_initkey','_cryptonite_aes_ocb_aad','_cryptonite_aes_ocb_decrypt','_cryptonite_aes_ocb_encrypt','_cryptonite_aes_ocb_finish','_cryptonite_aes_ocb_init','_cryptonite_blake2b_finalize','_cryptonite_blake2b_init','_cryptonite_blake2b_update','_cryptonite_cpu_has_rdrand','_cryptonite_curve25519_donna','_cryptonite_ed25519_publickey','_cryptonite_ed25519_sign','_cryptonite_ed25519_sign_open','_cryptonite_get_rand_bytes','_cryptonite_sha1_finalize','_cryptonite_sha1_init','_cryptonite_sha1_update','_cryptonite_sha3_finalize','_cryptonite_sha3_init','_cryptonite_sha3_update','_cryptonite_sha512_finalize','_cryptonite_sha512_init','_cryptonite_sha512_update','_cryptonite_skein512_finalize','_cryptonite_skein512_init','_cryptonite_skein512_update']"
EXPORTED_FUNCTIONS=""
FLAGS=""
for FUNC in $(cat cbits/exported.txt) ; do
  if [ "$EXPORTED_FUNCTIONS" == "" ] ; then
    EXPORTED_FUNCTIONS="["
  else
    EXPORTED_FUNCTIONS+=","
  fi
  EXPORTED_FUNCTIONS+="'"'_'$FUNC"'"
done
EXPORTED_FUNCTIONS+="]"

echo ${EXPORTED_FUNCTIONS}

CC="emcc -c -msse3 -O2 -I cbits/ -DARCH_X86_64"
$CC cbits/ed25519/*.c cbits/cryptonite_sha512.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC -maes cbits/aes/{generic,gf}.c cbits/cryptonite_aes.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC cbits/cryptonite_skein512.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC -I cbits/blake2/ref cbits/blake2/ref/*.c cbits/cryptonite_blake2b.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC cbits/curve25519/curve25519-donna.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC cbits/cryptonite_sha1.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
$CC cbits/cryptonite_sha3.c -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS && \
emcc -O2 -o cryptonite.js gf.o generic.o cryptonite_aes.o cryptonite_sha3.o cryptonite_sha1.o curve25519-donna.o cryptonite_blake2b.o blake2b-ref.o cryptonite_sha512.o cryptonite_skein512.o ed25519.o -s EXPORTED_FUNCTIONS=$EXPORTED_FUNCTIONS
#emcc -msse3 -maes -O2 -I -i cbits/ cbits/ed25519/*.c cbits/aes/*.c cbits/cryptonite_sha512.c -s EXPORTED_FUNCTIONS="['_cryptonite_aes_decrypt_cbc','_cryptonite_aes_decrypt_ecb','_cryptonite_aes_encrypt_cbc','_cryptonite_aes_encrypt_ctr','_cryptonite_aes_encrypt_ecb','_cryptonite_aes_gcm_aad','_cryptonite_aes_gcm_decrypt','_cryptonite_aes_gcm_encrypt','_cryptonite_aes_gcm_finish','_cryptonite_aes_gcm_init','_cryptonite_aes_initkey','_cryptonite_aes_ocb_aad','_cryptonite_aes_ocb_decrypt','_cryptonite_aes_ocb_encrypt','_cryptonite_aes_ocb_finish','_cryptonite_aes_ocb_init','_cryptonite_blake2b_finalize','_cryptonite_blake2b_init','_cryptonite_blake2b_update','_cryptonite_cpu_has_rdrand','_cryptonite_curve25519_donna','_cryptonite_ed25519_publickey','_cryptonite_ed25519_sign','_cryptonite_ed25519_sign_open','_cryptonite_get_rand_bytes','_cryptonite_sha1_finalize','_cryptonite_sha1_init','_cryptonite_sha1_update','_cryptonite_sha3_finalize','_cryptonite_sha3_init','_cryptonite_sha3_update','_cryptonite_sha512_finalize','_cryptonite_sha512_init','_cryptonite_sha512_update','_cryptonite_skein512_finalize','_cryptonite_skein512_init','_cryptonite_skein512_update']"
