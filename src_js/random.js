
var MyRandom = new class {
  constructor() {
    this.sha512 = function(data) {
      var encoder = new TextEncoder("ascii");
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
          current = encoder.encode(current);
        }
        Module.HEAPU8.set(current, stackData);
        _cryptonite_sha512_update(state, stackData, data[key].length);
      }
      _cryptonite_sha512_finalize(state, returnedData);
      var ret = Module.HEAPU8.subarray(returnedData, returnedData + 64);
      Runtime.stackRestore(stackTop);
      return ret;
    }
    this.state = new Uint8Array(64);
  }
  getSingleRandom() {
    var newRandom = new Uint8Array(64);
    window.crypto.getRandomValues(newRandom);
    var previous = this.state.subarray();
    this.state.set(this.sha512(["stat", previous, newRandom]));
    return this.sha512(["outp", previous, newRandom]);
  }
  getRandom(target) {
    if (typeof target === "number") {
      var result = new Uint8Array(target);
    } else {
      var result = target;
    }
    for (var i = 0; i < result.length; i += 64) {
      result.set(this.getSingleRandom().subarray(0, result.length - i), i);
    }
    if (typeof target === "number") {
      return new TextDecoder("ascii").decode(result);
    }
  }
  addSalt(salt) {
    var newRandom = new Uint8Array(64);
    window.crypto.getRandomValues(newRandom);
    var previous = this.state.subarray();
    var time = window.performance.now ? "" + window.performance.now() : "";
    var newState = [salt, previous, newRandom, time];
    this.state.set(this.sha512(newState));
  }
}

h$cryptonite_cpu_has_rdrand = function() { return 1; }
h$cryptonite_get_rand_bytes = function(ptr, ignore, count) {
  if (count == ptr.length) {
    MyRandom.getRandom(ptr);
  } else {
    var result = MyRandom.getRandom(count);
    ptr.set(result);
  }
}
