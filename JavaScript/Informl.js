////////////////////////////////////////////////////////////////
//
// Informl.js
//
// Module for JavaScript implementations of Informl primitives.
//
// Dependencies:
//   uxadt.js

////////////////////////////////////////////////////////////////
// Module definition.

var Informl = (function(uxadt){
  var Informl = {};
  
  Informl.size = function (o) {
    if (o == null) {
      return null;
    } else if (typeof o === 'string') {
      return o.length;
    } else if (Object.prototype.toString.call(o) === '[object Array]') {
      return o.length;
    } else {
      return null;
    }
  }

  return Informl;
}(uxadt));

//eof
