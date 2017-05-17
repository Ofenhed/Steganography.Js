function init_ghcjs_emscripten_bindings() {
function emscripten_ptr_to_haskell_ptr(ptr, /* optional */ length) {
  return h$wrapBuffer(Module.buffer, true, ptr, length);
}

function write_emscripten_ptr_to_haskell_ptr(e_ptr, h_ptr) {
  h_ptr.u8.set(Module.HEAPU8.subarray(e_ptr, e_ptr + h_ptr.len));
}

function ptr_(ptr, copy_before, copy_after) {
  if (ptr == null) {
    return [null];
  }
  var ret = Runtime.stackAlloc(ptr.len*2);
  if (copy_before) {
    Module.HEAPU8.set(ptr.u8, ret);
  }
  return copy_after ? [ret, function() {
    write_emscripten_ptr_to_haskell_ptr(ret, ptr);
  }] : [ret];
}

function heapptr_(ptr, copy_before, copy_after) {
  if (ptr == null) {
    return [null];
  }
  var ret = Module.allocate(ptr.u8, 'i8', ALLOC_NORMAL);
  if (ret == 0) {
    throw "Failed to allocate " + ptr.len + " bytes";
  }
  return [ret, function() {
    if (copy_after) {
      write_emscripten_ptr_to_haskell_ptr(ret, ptr);
    }
    Module._free(ret);
  }];
}

var c_ptr = function(x) { return heapptr_(x, true, false); }
var ptr = function(x) { return heapptr_(x, true, true); }
var o_ptr = function(x) { return heapptr_(x, false, true); }
c_ptr = ptr;
o_ptr = ptr;
var dc_ptr = c_ptr;

function fwd(x) {
  return [x];
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
  return [arr, function() { for (var f in finalizer) finalizer[f]() }];
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

h$cryptonite_aes_encrypt_ctr = runFunc(_cryptonite_aes_encrypt_ctr, "cryptonite_aes_encrypt_ctr", [o_ptr, null, c_ptr, null, c_ptr, null, c_ptr, null, fwd])
h$cryptonite_aes_initkey = runFunc(_cryptonite_aes_initkey, "cryptonite_aes_initkey", [ptr, null, ptr, null, fwd])
h$cryptonite_blake2b_finalize = runFunc(_cryptonite_blake2b_finalize, "cryptonite_blake2b_finalize", [dc_ptr, null, fwd, o_ptr, null])
h$cryptonite_blake2b_init = runFunc(_cryptonite_blake2b_init, "cryptonite_blake2b_init", [o_ptr, null, fwd])
h$cryptonite_blake2b_update = runFunc(_cryptonite_blake2b_update, "cryptonite_blake2b_update", [ptr, null, c_ptr, null, fwd])
h$cryptonite_curve25519_donna = runFunc(_cryptonite_curve25519_donna, "cryptonite_curve25519_donna", [ptr, null, c_ptr, null, c_ptr, null])
h$cryptonite_ed25519_publickey = runFunc(_cryptonite_ed25519_publickey, "cryptonite_ed25519_publickey", [c_ptr, null, o_ptr, null])
h$cryptonite_ed25519_sign_open = runFunc(_cryptonite_ed25519_sign_open, "cryptonite_ed25519_sign_open", [c_ptr, null, fwd, c_ptr, null, c_ptr, null])
h$cryptonite_ed25519_sign = runFunc(_cryptonite_ed25519_sign, "cryptonite_ed25519_sign", [c_ptr, null, fwd, c_ptr, null, c_ptr, null, o_ptr, null])
h$cryptonite_sha1_finalize = runFunc(_cryptonite_sha1_finalize, "cryptonite_sha1_finalize", [dc_ptr, null, o_ptr, null])
h$cryptonite_sha1_init = runFunc(_cryptonite_sha1_init, "cryptonite_sha1_init", [o_ptr, null]);
h$cryptonite_sha1_update = runFunc(_cryptonite_sha1_update, "cryptonite_sha1_update", [ptr, null, c_ptr, null, fwd])
h$cryptonite_sha3_finalize = runFunc(_cryptonite_sha3_finalize, "cryptonite_sha3_finalize", [dc_ptr, null, fwd, o_ptr, null])
h$cryptonite_sha3_init = runFunc(_cryptonite_sha3_init, "cryptonite_sha3_init", [o_ptr, null, fwd])
h$cryptonite_sha3_update = runFunc(_cryptonite_sha3_update, "cryptonite_sha3_update", [ptr, null, c_ptr, null, fwd])
h$cryptonite_sha512_finalize = runFunc(_cryptonite_sha512_finalize, "cryptonite_sha512_finalize", [dc_ptr, null, o_ptr, null])
h$cryptonite_sha512_init = runFunc(_cryptonite_sha512_init, "cryptonite_sha512_init", [o_ptr, null])
h$cryptonite_sha512_update = runFunc(_cryptonite_sha512_update, "cryptonite_sha512_update", [ptr, null, c_ptr, null, fwd])
h$cryptonite_skein512_finalize = runFunc(_cryptonite_skein512_finalize, "cryptonite_skein512_finalize", [dc_ptr, null, fwd, o_ptr, null])
h$cryptonite_skein512_init = runFunc(_cryptonite_skein512_init, "cryptonite_skein512_init", [o_ptr, null, fwd])
h$cryptonite_skein512_update = runFunc(_cryptonite_skein512_update, "cryptonite_skein512_update", [ptr, null, c_ptr, null, fwd])
};
