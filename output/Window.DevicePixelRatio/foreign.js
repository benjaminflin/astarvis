exports.devicePixelRatio = function(window) {
  return function() {
    return window.devicePixelRatio || 1;
  };
};
