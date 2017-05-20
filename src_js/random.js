function init_random(Module) {
  function stringToUint(string) {
    if (typeof this.encoder !== "undefined") {
      return this.encoder.encode(string);
    }
    if (typeof TextEncoder !== "undefined") {
      this.encoder = new TextEncoder("ascii");
      return stringToUint(string);
    }
    var charList = string.split(''),
      uintArray = [];
    for (var i = 0; i < charList.length; i++) {
      uintArray.push(charList[i].charCodeAt(0));
    }
    return new Uint8Array(uintArray);
  }

  function uintToString(uintArray) {
    if (typeof this.decoder !== "undefined") {
      return this.decoder.decode(uintArray);
    }
    if (typeof TextDecoder !== "undefined") {
      this.decoder = new TextDecoder("ascii");
      return uintToString(uintArray);
    }
    var decodedString = String.fromCharCode.apply(null, uintArray);
    return decodedString;
  }
  // Sha512 stuff, including temporary state and temporary input buffer
  var sha512_state = Module.allocate(new Uint8Array(256), 'i8', Module.ALLOC_NORMAL);
  var currentInputStateLength = 1024;
  var currentInputState = Module.allocate(new Uint8Array(currentInputStateLength), 'i8', Module.ALLOC_NORMAL);
  function assertInputBufLength(length) {
    if (length > currentInputState) {
      Module._free(currentInputState);
      currentInputState = Module.alloc(new Uint8Array(currentInputStateLength = length), 'i8', Module.ALLOC_NORMAL);
    }
    return currentInputState;
  }
  function sha512(data, out) {
      var maxlength = 0;
      for (var key in data) {
        if (data[key].length > maxlength) {
          maxlength = data[key].length + 1;
        }
      }
      Module._cryptonite_sha512_init(sha512_state);
      for (var key in data) {
        var current = data[key];
        if (typeof current === "object" &&
            current.length == 2 &&
            typeof current[0] === "number" &&
            typeof current[1] === "number") {
          Module._cryptonite_sha512_update(sha512_state, current[0], current[1]);
        } else {
          if (typeof current === "string") {
            current = stringToUint(current);
          }
          assertInputBufLength(current.length);
          Module.HEAPU8.set(current, currentInputState);
          Module._cryptonite_sha512_update(sha512_state, currentInputState, data[key].length);
        }
      }
      Module._cryptonite_sha512_finalize(sha512_state, out);
  }
  // resultMem, an allocated buffer for getting the result from sha512
  var resultMem = Module.allocate(new Uint8Array(64), 'i8', Module.ALLOC_NORMAL);
  // state holds the current "random" state
  var state = Module.allocate(new Uint8Array(64), 'i8', Module.ALLOC_NORMAL);
  // tmpSaltRandom is simply a buffer to avoid it being allocated every time salt is added
  var tmpSaltRandom = new Uint8Array(64);
  var MyRandom = {
    getBrowserRandom: function(target) {
      if (window.crypto && window.crypto.getRandomValues) {
        window.crypto.getRandomValues(target);
      } else if (typeof this.notifiedAboutMissingRandom === "undefined") {
        this.notifiedAboutMissingRandom = true;
        alert("This browser doesn't provide any cryptographically secure random.");
      }
    },
    getSingleRandom: function() {
      var newRandom = new Uint8Array(64);
      this.getBrowserRandom(newRandom);
      sha512(["outp", [state, 64], newRandom], resultMem);
      sha512(["stat", [state, 64], newRandom], state);
      newRandom.set(Module.HEAPU8.subarray(resultMem, resultMem + 64));
      return newRandom;
    },
    getRandom: function(target) {
      if (typeof target === "number") {
        var result = new Uint8Array(target);
      } else {
        var result = target;
      }
      for (var i = 0; i < result.length; i += 64) {
        result.set(this.getSingleRandom().subarray(0, result.length - i), i);
      }
      if (typeof target === "number") {
        return uintToString(result);
      }
    },
    addSalt: function(salt) {
      this.getBrowserRandom(tmpSaltRandom);
      var time = window.performance.now ? "" + window.performance.now() : new Date().toString();
      var newState = [salt, [state, 64], tmpSaltRandom, time];
      sha512(newState, state);
    }
  }

  h$cryptonite_cpu_has_rdrand = function() { return 1; }
  h$cryptonite_get_rand_bytes = function(ptr, ignore, count) {
    if (count == ptr.length) {
      MyRandom.getRandom(ptr);
    } else {
      var result = new Uint8Array(count);
      MyRandom.getRandom(result);
      ptr.u8.set(result);
    }
    return count;
  }
  return MyRandom;
}
