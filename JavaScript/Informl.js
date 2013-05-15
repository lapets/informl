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
  
  //////////////////////////////////////////////////////////////
  // Primitive built-in operations.
  
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

  Informl.plus = function (o1, o2) {
    if (o1 == null || o2 == null) {
      return null;
    } else if (typeof o1 === 'string' && typeof o2 === 'string') {
      return o1.concat(o2);
    } else if (Object.prototype.toString.call(o1) === '[object Array]' 
             && Object.prototype.toString.call(o2) === '[object Array]') {
      return o1.concat(o2);
    } else {
      return null;
    }
  }

  //////////////////////////////////////////////////////////////
  // Library operations and functions.
  
  Informl.__suffix_of_X_after_index_X = function(o, i) {
    if (typeof o === 'string')
       return o.substring(i);

    if (Object.prototype.toString.call(o) === '[object Array]') {
      var a = []; for (j = i; j < o.length; j++) a.push(o[j]);
      return a;
    }

    return null;
  }
  
  Informl.__trim_whitespace_from_X = function(s) {
    return s.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
  }

  Informl.__X_is_prefix_of_X = function(s, t) {
    for (var i = 0; i < s.length && i < t.length; i++)
      if (i >= s.length || s[i] != t[i])
        return false;
    return true;
  }

  return Informl;
}(uxadt));

//eof
