exports.worker = function(url) {
  return new Worker(url);
};

exports.postMessage = function(worker) {
  return function(message) {
    return function() {
      worker.postMessage(message);
    };
  };
};

exports.globalScope = function() {
  return function() {
    return self;
  };
};

exports.postMessageGlobal = function(global) {
  return function(message) {
    return function() {
      global.postMessage(message);
    };
  };
};

exports.onMessage = function(worker) {
  return function(callback) {
    return function() {
      worker.onmessage = function(event) {
        callback(event.data);
      };
    };
  };
};

exports.onMessageGlobal = function(global) {
  return function(callback) {
    return function() {
      global.onmessage = function(event) {
        callback(event.data);
      };
    };
  };
};

exports.terminate = function(worker) {
  return function() {
    worker.terminate();
  };
};
