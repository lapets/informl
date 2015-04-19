/********************************************************************
** 
** uxadt_old.js
**
**   A library that supports a universal, cross-platform embedded
**   representation for algebraic data type (ADT) values, and a
**   programming abstraction for operations (such as pattern
**   matching) on algebraic data type values.
**
**   Web:     uxadt.org
**   Version: 0.0.0.2
**
*/

(function (_, uxadt_old) {

  "use strict";


  // Wildcard pattern is _.
  uxadt_old._ = _;

  // Representation for individual algebraic data type values
  // and value patterns.
  uxadt_old.Value = function() {};

  // Pattern matching unification algorithm.
  uxadt_old.unify = 
    function (p, v) {
      if (p == uxadt_old._)
        return [v];

      for (var c in p) {
        for (var d in v) {
          if ( !(c in uxadt_old.Value.prototype) 
            && !(d in uxadt_old.Match.prototype) 
            && c == d 
            && p[c].length == v[d].length
             ) {
            var substs = [];
            for (var i = 0; i < p[c].length; i++) {
              var subst = uxadt_old.unify(p[c][i], v[d][i]);
              if (subst == null)
                return null;
              substs = substs.concat(subst);
            }
            return substs;
          }
        }
      }
      return null; // Failure.
    }

  // Constructor and interface for pattern matching results.
  uxadt_old.Match = 
    function (value) {
      this.isMatch = true;
      this.end = value;
    };

  uxadt_old.Match.prototype.match =
    function (p, f) {
      return this;
    }
  uxadt_old.Match.prototype._ = uxadt_old.Match.prototype.match;

  // Interface for matching algebraic data type values.
  uxadt_old.Value.prototype.match =
    function (p, f) {
      var subst = uxadt_old.unify(p, this);
      return (subst != null) ? new uxadt_old.Match(f.apply(f, subst)) : this;
    }
  uxadt_old.Value.prototype._ = uxadt_old.Value.prototype.match;

  // Algebraic data type value constructor.
  uxadt_old.constructor = function (name) {
    return (
        function () {
          var value = new uxadt_old.Value();
          value[name] = [];
          for (var i = 0; i < arguments.length; i++)
            value[name].push(arguments[i]);
          return value;
        }
      );
  }

  // Function for introducing a new algebraic data type
  // (can introduce constructors into specified scope).
  uxadt_old.define = function () {
    var cases, obj;
    if (arguments.length == 1) {
      obj = {};
      cases = arguments[0];
    } else if (arguments.length == 2) {
      obj = arguments[0];
      cases = arguments[1];
    }

    for (var con in cases)
      obj[con] = (cases[con].length == 0) ? uxadt_old.constructor(con) : uxadt_old.constructor(con);

    return obj;
  }

//})(typeof exports !== 'undefined' ? exports : (this.uxadt_old = {}));
//})(this.uxadt_old = (function(){}));
})(_, this.uxadt_old = 

  // Extensions on existing prototypes for handling common operations
  // on container types that may hold uxadt_old Values.

  function __uxadt_old__(obj) {

    // Return a new, wrapped object on which uxadt_old functions are defined.
    var self = this;
    obj = obj == null ? self : obj;

    // Apply the function to the first element in an array that matches
    // the specified pattern (i.e., a 'find' operation) and returns a
    // non-null result value.
    obj.find =
      function (p, f) {
        for (var i = 0; i < obj.length; i++) {
          var value = obj[i]._(p, f).end;
          if (value != null)
            return value;
        }
        return null;
      };

    // Return a new array containing the NON-NULL results of applying 
    // the specified function to each element in the input array that
    // matches the specified pattern (i.e., a 'filter' operation
    // and a 'map' operation).
    obj.map =
      function (p, f) {
        var a = [];
        for (var i = 0; i < obj.length; i++) {
          var value = obj[i]._(p, f).end;
          if (value != null)
            a.push(value);
        }
        return uxadt_old(a);
      };

    return obj;
  }
);

/////////////////////////////////////////////////////////////////////
// Useful global synonyms.

if (typeof _ == 'undefined')
  var _ = null;

/*
// Example.
function hgt(t) {
  return t
     ._(Leaf(), function(){
          return 1;
       })
     ._(Node(_, _), function(x, y){
          return hgt(x) + hgt(y);
       })
     .end;
}*/

/* eof */