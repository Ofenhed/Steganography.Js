function emscripten_ptr_to_haskell_ptr(ptr, /* optional */ length) {
  var str = Pointer_stringify(ptr, length);
  var iArr = intArrayFromString(str, true);
  _free(str);
  var array = h$newByteArray(iArr.length);
  array.u8.set(iArr);
  return array;
}

function write_emscripten_ptr_to_haskell_ptr(e_ptr, h_ptr, length) {
  h_ptr.u8.set(Module.HEAPU8.subarray(e_ptr, e_ptr + length));
}

function c_ptr(ptr) {
  var ret = Runtime.stackAlloc(ptr.len);
  Module.HEAPU8.set(ptr.u8, ret);
  return [ret];
}

function ptr(ptr) {
  if (ptr == null) {
    return [null];
  }
  var ret = Runtime.stackAlloc(ptr.len);
  Module.HEAPU8.set(ptr.u8, ret);
  return [ret, function() {
    write_emscripten_ptr_to_haskell_ptr(ret, ptr, ptr.len);
  }];
}

function zipMap(a, b) {
  var arr = [];
  var finalizer = [];
  for (var key in a)
    if (b[key] != null) {
      var res = b[key](a[key]);
      if (res.length == 2) {
        finalizer.push(res[1]);
      }
      arr.push(res[0]);
    }
  return [arr, function() { for (var f in finalizer.reverse()) finalizer[f]() }];
}

function fwd(x) {
  return [x];
}

function runFunc(func, funcname, operations) {
  return function() {
    if (arguments.length != operations.length) {
      alert("Arguments missmatch for function " + funcname);
    }
    var stackTop = Runtime.stackSave();
    var zipped = zipMap(arguments, operations);
    var ret = func.apply(this, zipped[0]);
    zipped[1]();
    Runtime.stackRestore(stackTop);
    return ret;
  }
}

function str2ab(str) {
  var buf = new ArrayBuffer(str.length*2); // 2 bytes for each char
  var bufView = new Uint16Array(buf);
  for (var i=0, strLen=str.length; i<strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

//h$deflate             =runFunc(_deflate,             "deflate");
//h$deflateInit2_       =runFunc(_deflateInit2_,       "deflateInit2_"); // Module.cwrap('deflateInit2_', 'i32', ['*', 'i32', 'i32', 'i32', 'i32', 'i32', 'string', 'i32']);
//h$inflateEnd          =runFunc(_inflateEnd,          "inflateEnd");
////h$inflateInit2_       =runFunc(_inflateInit2_,       "inflateInit2_");
//h$inflateReset        =runFunc(_inflateReset,        "inflateReset");
//h$inflateSetDictionary=runFunc(_inflateSetDictionary,"inflateSetDictionary");
////h$zlibVersion         =function() {
////  return emscripten_ptr_to_haskell_ptr(_zlibVersion());
////};
//h$inflateInit2_             =function(v1, v2 ,v3 ,v4, v5, v6) {
//  o1 = haskell_ptr_to_emscripten_ptr(v1);
//  o4 = haskell_ptr_to_emscripten_ptr(v4);
//  result = _inflateInit2_(o1, v3, o4, v6);
//  _free(o1);
//  _free(o4);
//  alert(result);
//  return result;
//}
//h$inflate             =function(v1, v2, v3) {
//  o1 = haskell_ptr_to_emscripten_ptr(v1);
//  return _inflate(o1, v3);
//}
//
//


h$cryptonite_sha1_init = runFunc(_cryptonite_sha1_init, "cryptonite_sha1_init", [ptr, null]);
h$cryptonite_sha1_update = runFunc(_cryptonite_sha1_update, "cryptonite_sha1_update", [ptr, null, c_ptr, null, fwd])
h$cryptonite_sha1_finalize = runFunc(_cryptonite_sha1_finalize, "cryptonite_sha1_finalize", [c_ptr, null, ptr, null])
h$cryptonite_aes_decrypt_cbc = runFunc(_cryptonite_aes_decrypt_cbc, "cryptonite_aes_decrypt_cbc", [ptr, null, ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_encrypt_cbc = runFunc(_cryptonite_aes_encrypt_cbc, "cryptonite_aes_encrypt_cbc", [ptr, null, ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_decrypt_ecb = runFunc(_cryptonite_aes_decrypt_ecb, "cryptonite_aes_decrypt_ecb", [ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_encrypt_ecb = runFunc(_cryptonite_aes_encrypt_ecb, "cryptonite_aes_encrypt_ecb", [ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_encrypt_ctr = runFunc(_cryptonite_aes_encrypt_ctr, "cryptonite_aes_encrypt_ctr", [ptr, null, ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_aes_gcm_aad = runFunc(_cryptonite_aes_gcm_aad, "cryptonite_aes_gcm_aad", [ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_gcm_decrypt = runFunc(_cryptonite_aes_gcm_decrypt, "cryptonite_aes_gcm_decrypt", [ptr, null, ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_gcm_encrypt = runFunc(_cryptonite_aes_gcm_encrypt, "cryptonite_aes_gcm_encrypt", [ptr, null, ptr, null, ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_gcm_finish = runFunc(_cryptonite_aes_gcm_finish, "cryptonite_aes_gcm_finish", [ptr, null, ptr, null, ptr, null])
h$cryptonite_aes_gcm_init = runFunc(_cryptonite_aes_gcm_init, "cryptonite_aes_gcm_init", [ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_aes_initkey = runFunc(_cryptonite_aes_initkey, "cryptonite_aes_initkey", [ptr, null, ptr, null, fwd])
h$cryptonite_aes_ocb_aad = runFunc(_cryptonite_aes_ocb_aad, "cryptonite_aes_ocb_aad", [ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_aes_ocb_decrypt = runFunc(_cryptonite_aes_ocb_decrypt, "cryptonite_aes_ocb_decrypt", [ptr, null, ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_aes_ocb_encrypt = runFunc(_cryptonite_aes_ocb_encrypt, "cryptonite_aes_ocb_encrypt", [ptr, null, ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_aes_ocb_finish = runFunc(_cryptonite_aes_ocb_finish, "cryptonite_aes_ocb_finish", [ptr, null, ptr, null, ptr, null])
h$cryptonite_aes_ocb_init = runFunc(_cryptonite_aes_ocb_init, "cryptonite_aes_ocb_init", [ptr, null, ptr, null, ptr, null, fwd])
h$cryptonite_blake2b_finalize = runFunc(_cryptonite_blake2b_finalize, "cryptonite_blake2b_finalize", [ptr, null, fwd, ptr, null])
h$cryptonite_blake2b_init = runFunc(_cryptonite_blake2b_init, "cryptonite_blake2b_init", [ptr, null, fwd])
h$cryptonite_blake2b_update = runFunc(_cryptonite_blake2b_update, "cryptonite_blake2b_update", [ptr, null, ptr, null, fwd])
h$cryptonite_curve25519_donna = runFunc(_cryptonite_curve25519_donna, "cryptonite_curve25519_donna", [ptr, null, ptr, null, ptr, null])
h$cryptonite_sha3_finalize = runFunc(_cryptonite_sha3_finalize, "cryptonite_sha3_finalize", [ptr, null, fwd, ptr, null])
h$cryptonite_sha3_init = runFunc(_cryptonite_sha3_init, "cryptonite_sha3_init", [ptr, null, fwd])
h$cryptonite_sha3_update = runFunc(_cryptonite_sha3_update, "cryptonite_sha3_update", [ptr, null, ptr, null, fwd])
h$cryptonite_sha512_finalize = runFunc(_cryptonite_sha512_finalize, "cryptonite_sha512_finalize", [ptr, null, ptr, null])
h$cryptonite_sha512_init = runFunc(_cryptonite_sha512_init, "cryptonite_sha512_init", [ptr, null])
h$cryptonite_sha512_update = runFunc(_cryptonite_sha512_update, "cryptonite_sha512_update", [ptr, null, ptr, null, fwd])
h$cryptonite_skein512_finalize = runFunc(_cryptonite_skein512_finalize, "cryptonite_skein512_finalize", [ptr, null, fwd, ptr, null])
h$cryptonite_skein512_init = runFunc(_cryptonite_skein512_init, "cryptonite_skein512_init", [ptr, null, fwd])
h$cryptonite_skein512_update = runFunc(_cryptonite_skein512_update, "cryptonite_skein512_update", [ptr, null, ptr, null, fwd])