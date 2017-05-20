function init_ghcjs_emscripten_bindings(Module) {
  var dataBufferLength = 32;
  var dataBuffer = Module.allocate(dataBufferLength, 'i8', Module.ALLOC_NORMAL);
  var dataBufferUsed = 0;
  function increaseBufferSizeByAtLeast(size) {
    let newDataBuffer = Module.allocate(dataBufferLength = 2**Math.ceil(Math.log2(dataBufferLength + size)), 'i8', Module.ALLOC_NORMAL);
    if (dataBufferUsed) {
      var sub = Module.HEAPU8.slice(dataBuffer, dataBuffer + dataBufferUsed);
      Module.HEAPU8.set(sub, newDataBuffer);
    }
    Module._free(dataBuffer);
    dataBuffer = newDataBuffer;
  }
  function emscripten_ptr_to_haskell_ptr(ptr, /* optional */ length) {
    return h$wrapBuffer(Module.buffer, true, ptr, length);
  }

  function write_emscripten_ptr_to_haskell_ptr(e_ptr, h_ptr) {
    var sub = Module.HEAPU8.subarray(e_ptr, e_ptr + h_ptr.u8.length);
    h_ptr.u8.set(sub);
  }

  function write_haskell_ptr_to_emscripten_ptr(h_ptr, e_ptr, length) {
    if (typeof length === "number") {
      Module.HEAPU8.set(h_ptr.u8, e_ptr, e_ptr + length);
    } else {
      Module.HEAPU8.set(h_ptr.u8, e_ptr);
    }
  }

  function ptr_(ptr, copy_before, copy_after) {
    if (ptr == null) {
      return [0, null];
    }
    var ret = dataBuffer + dataBufferUsed;
    if (dataBufferUsed + ptr.u8.length > dataBufferLength) {
      increaseBufferSizeByAtLeast(ptr.u8.length);
    }
    dataBufferUsed += ptr.u8.length;
    if (copy_before) {
      write_haskell_ptr_to_emscripten_ptr(ptr, ret);
    }
    return copy_after ? [ptr.u8.length, ret, function() {
      write_emscripten_ptr_to_haskell_ptr(ret, ptr);
    }] : [ptr.u8.length, ret];
  }

  var c_ptr = function(x) { return ptr_(x, true, false); }
  var ptr = function(x) { return ptr_(x, true, true); }
  var o_ptr = function(x) { return ptr_(x, false, true); }
  c_ptr = ptr;
  o_ptr = ptr;
  var dc_ptr = c_ptr;

  function fwd(x) {
    return [0, x];
  }


  function zipMap(a, b) {
    var arr = [];
    var finalizer = [];
    var size = 0;
    for (var key in a)
      if (b[key] != null) {
        var res = b[key](a[key]);
        if (res.length == 3) {
          finalizer.push(res[2]);
        }
        arr.push(res[1]);
        size += res[0]
      }
    return [size, arr, function() { for (var f in finalizer) finalizer[f]() }];
  }

  function runFunc(func, funcname, operations) {
    return function() {
      if (arguments.length != operations.length) {
        alert("Arguments missmatch for function " + funcname);
      }
      dataBufferUsed = 0;
      var zipped = zipMap(arguments, operations);
      var ret = func.apply(this, zipped[1]);
      zipped[2]();
      return ret;
    }
  }

  h$cryptonite_aes_encrypt_ctr = runFunc(Module._cryptonite_aes_encrypt_ctr, "cryptonite_aes_encrypt_ctr", [o_ptr, null, c_ptr, null, c_ptr, null, c_ptr, null, fwd])
  h$cryptonite_aes_initkey = runFunc(Module._cryptonite_aes_initkey, "cryptonite_aes_initkey", [ptr, null, ptr, null, fwd])
  h$cryptonite_blake2b_finalize = runFunc(Module._cryptonite_blake2b_finalize, "cryptonite_blake2b_finalize", [dc_ptr, null, fwd, o_ptr, null])
  h$cryptonite_blake2b_init = runFunc(Module._cryptonite_blake2b_init, "cryptonite_blake2b_init", [o_ptr, null, fwd])
  h$cryptonite_blake2b_update = runFunc(Module._cryptonite_blake2b_update, "cryptonite_blake2b_update", [ptr, null, c_ptr, null, fwd])
  h$cryptonite_curve25519_donna = runFunc(Module._cryptonite_curve25519_donna, "cryptonite_curve25519_donna", [ptr, null, c_ptr, null, c_ptr, null])
  h$cryptonite_ed25519_publickey = runFunc(Module._cryptonite_ed25519_publickey, "cryptonite_ed25519_publickey", [c_ptr, null, o_ptr, null])
  h$cryptonite_ed25519_sign_open = runFunc(Module._cryptonite_ed25519_sign_open, "cryptonite_ed25519_sign_open", [c_ptr, null, fwd, c_ptr, null, c_ptr, null])
  h$cryptonite_ed25519_sign = runFunc(Module._cryptonite_ed25519_sign, "cryptonite_ed25519_sign", [c_ptr, null, fwd, c_ptr, null, c_ptr, null, o_ptr, null])
  h$cryptonite_sha1_finalize = runFunc(Module._cryptonite_sha1_finalize, "cryptonite_sha1_finalize", [dc_ptr, null, o_ptr, null])
  h$cryptonite_sha1_init = runFunc(Module._cryptonite_sha1_init, "cryptonite_sha1_init", [o_ptr, null]);
  h$cryptonite_sha1_update = runFunc(Module._cryptonite_sha1_update, "cryptonite_sha1_update", [ptr, null, c_ptr, null, fwd])
  h$cryptonite_sha3_finalize = runFunc(Module._cryptonite_sha3_finalize, "cryptonite_sha3_finalize", [dc_ptr, null, fwd, o_ptr, null])
  h$cryptonite_sha3_init = runFunc(Module._cryptonite_sha3_init, "cryptonite_sha3_init", [o_ptr, null, fwd])
  h$cryptonite_sha3_update = runFunc(Module._cryptonite_sha3_update, "cryptonite_sha3_update", [ptr, null, c_ptr, null, fwd])
  h$cryptonite_sha512_finalize = runFunc(Module._cryptonite_sha512_finalize, "cryptonite_sha512_finalize", [dc_ptr, null, o_ptr, null])
  h$cryptonite_sha512_init = runFunc(Module._cryptonite_sha512_init, "cryptonite_sha512_init", [o_ptr, null])
  h$cryptonite_sha512_update = runFunc(Module._cryptonite_sha512_update, "cryptonite_sha512_update", [ptr, null, c_ptr, null, fwd])
  h$cryptonite_skein512_finalize = runFunc(Module._cryptonite_skein512_finalize, "cryptonite_skein512_finalize", [dc_ptr, null, fwd, o_ptr, null])
  h$cryptonite_skein512_init = runFunc(Module._cryptonite_skein512_init, "cryptonite_skein512_init", [o_ptr, null, fwd])
  h$cryptonite_skein512_update = runFunc(Module._cryptonite_skein512_update, "cryptonite_skein512_update", [ptr, null, c_ptr, null, fwd])
};
