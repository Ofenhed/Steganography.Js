(function() {
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
  MyRandom = {
    sha512: function(data) {
      var stackTop = Runtime.stackSave();
      var state = Runtime.stackAlloc(256);
      var maxlength = 0;
      for (var key in data) {
        if (data[key].length > maxlength) {
          maxlength = data[key].length + 1;
        }
      }
      var stackData = Runtime.stackAlloc(maxlength);
      var returnedData = Runtime.stackAlloc(64);
      _cryptonite_sha512_init(state);
      for (var key in data) {
        var current = data[key];
        if (typeof current === "string") {
          current = stringToUint(current);
        }
        Module.HEAPU8.set(current, stackData);
        _cryptonite_sha512_update(state, stackData, data[key].length);
      }
      _cryptonite_sha512_finalize(state, returnedData);
      var ret = Module.HEAPU8.subarray(returnedData, returnedData + 64);
      Runtime.stackRestore(stackTop);
      return ret;
    },
    getBrowserRandom: function(target) {
      if (window.crypto && window.crypto.getRandomValues) {
        window.crypto.getRandomValues(target);
      } else if (typeof this.notifiedAboutMissingRandom === "undefined") {
        this.notifiedAboutMissingRandom = true;
        alert("This browser doesn't provide any cryptographically secure random.");
      }
    },
    state: new Uint8Array(64),
    getSingleRandom: function() {
      var newRandom = new Uint8Array(64);
      this.getBrowserRandom(newRandom);
      var result = this.sha512(["outp", this.state, newRandom]);
      this.state.set(this.sha512(["stat", this.state, newRandom]));
      return result;
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
      var newRandom = new Uint8Array(64);
      this.getBrowserRandom(newRandom);
      var time = window.performance.now ? "" + window.performance.now() : "";
      var newState = [salt, this.state, newRandom, time];
      this.state.set(this.sha512(newState));
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
})();
