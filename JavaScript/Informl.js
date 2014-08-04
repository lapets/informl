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

var Informl = (function(uxadt,_){
  var Informl = {};

  ///////////////////////////////////////////////////////////////
  // anonymous
  uxadt.qualified('anonymous',
  {
    'anonymous_0':[],
    'anonymous_1':['item'],
    'anonymous_2':['item','item'],
    'anonymous_3':['item','item','item'],
    'anonymous_4':['item','item','item','item'],
    'anonymous_5':['item','item','item','item','item'],
    'anonymous_6':['item','item','item','item','item','item']
  }
  );
  
  //////////////////////////////////////////////////////////////
  // Primitive built-in operations.
  Informl.unpack = function(o) {
    if(_.isObject(o) && (o instanceof uxadt.Value))
      for (var c in o) {
        if(c[0] != '_' && c[1] != '_')
          if (o[c].length == 0) return o;
          else return o[c];
      }
    else return o;
  }

  Informl.size = function (o) {
    if (o == null) {
      return null;
    } 
    else if (typeof o === 'string') {
      return o.length;
    } 
    else if (_.isArray(o)) {
      return o.length;
    } 
    else if (_.isObject(o) && (o instanceof uxadt.Value)) {
      return Informl.unpack(o).length;
    }
    else {
      return null;
    }
  }

  Informl.unitlist = function(o) {
    if (_.isArray(o)) {
      var flat = _.flatten(o);
      var retarr = [];
      for (var i in flat) {
        retarr.concat(Informl.unitlist(i));
      }
      return retarr;
    }
    if (_.isObject(o) && (o instanceof uxadt.Value)) {
      var flat = Informl.unpack(o);
      var retarr = [];
      if(!(_.isArray(flat))) flat = [flat]
      for (var i in flat) {
        retarr.concat(Informl.unitlist(i));
      }
      return retarr;
    }
    return [o];
  }

  Informl.plus = function (o1, o2) {
    if (o1 == null || o2 == null) {
      return null;
    } 
    else if (typeof o1 === 'number' && typeof o2 === 'number') {
      return o1 + o2;
    } 
    else if (typeof o1 === 'string' && typeof o2 === 'string') {
      return o1.concat(o2);
    } 
    else if (typeof o1 === 'number' && typeof o2 === 'string'){
      return o1.toString().concat(o2);
    } 
    else if (typeof o1 === 'string' && typeof o2 === 'number'){
      return o1.concat(o2.toString());
    } 
    else if (_.isArray(o1) && _.isArray(o2)) {
      return o1.concat(o2);
    } 
    else if (_.isArray(o1)) {
      return o1.concat([o2]);
    } 
    else if (_.isArray(o2)) {
      return o2.unshift(o1);
    } 
    else {
      return null;
    }
  }

  Informl.range = function(s,f) {
    return _.range(s,f);
  }

  Informl.join = function(l,s) {
    if (typeof s === 'string' && _.isArray(l))
      return l.join(s);
    else return null;
  }

  Informl.minus = function(o1,o2) {
    if (o1 == null || o2 == null) {
      return null;
    } 
    else if (typeof o1 === 'number' && typeof o2 === 'number') {
      return o1 - o2;
    }
    else if (_.isArray(o1) && _.isArray(o2)) {
      return _.difference(o1,o2);
    } 
    else if (_.isArray(o1)) {
      return _.without(o1,o2);
    } 
    else if (_.isObject) {
      return _.omit(o1,o2);
    } 
    else {
      return null;
    }
  }

  Informl.map = function(o,func) {
    if (o == null || func == null || !(_.isFunction(func))) {
      return null;
    }
    else if (_.isArray(o)) {
      return _.map(o,func)
    }
    else if (_.isObject(o) && (o instanceof uxadt.Value)) {
      for (var c in o) {
        if(c[0] != '_' && c[1] != '_') {
          return _.map(o[c],func);
        }
      }
      return null;
    }
    else {
      return null;
    }
  }

  Informl.fold = function(o,func,base) {
    if (o == null || func == null || !(_.isFunction(func))) {
      return null;
    }
    else if (_.isArray(o)) {
      return _.reduce(o,func,base)
    }
    else if (_.isObject(o) && (o instanceof uxadt.Value)) {
      for (var c in o) {
        if(c[0] != '_' && (c.length < 2 || c[1] != '_')) {
          return _.reduce(o[c],func,base);
        }
      }
      return null;
    }
    else {
      return null;
    }
  }

  Informl.type = function(t) {
    if (typeof t === 'number') return "number";
    if (typeof t === 'string') return "string";
    if (typeof t === 'boolean') return "boolean";
    if (_.isFunction(t)) return "function";
    if (_.isArray(t)) return "list";
    if (_.isObject(t) && (t instanceof uxadt.Value)) return "pattern";
    if (_.isObject(t)) return "dictionary";
    return "nothing";
  }

  Informl.listToDict = function(l) {
    if(!(_.isArray(l))) return null;
    var ret = {};
    for(var i = 0; i < l.length; i++) {
      ret[i.toString()] = l[i];
    }
    return ret;
  }

  Informl.dictToPattern = function(dict, many) {
    if (!_.isObject(dict)) return dict;
    var ux;
    for (var key in dict) {
      var list = [];

      var tmp = {};
      tmp[key] = list;
      uxadt._(tmp);
      eval("ux=".concat(key).concat("(").concat(Informl.join(list,",")).concat(");"));
      var mapRecusive = _.map(dict[key],function(x){return Informl.dictToPattern(x,many)});
      if (_.contains(many,key)) ux[key] = [mapRecusive];
      else ux[key] = mapRecusive;
    }
    return ux;
  }

  Informl.slice = function(c,start,end) {
    if (!(_.isArray(c) || typeof c === 'string')) return null
    if (end == null) end = c.length;
    if (start == null) start = 0;
    if (typeof c === 'string') 
      return c.substring(start,end);
    return c.slice(start,end);
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
}(uxadt,_));

//eof
