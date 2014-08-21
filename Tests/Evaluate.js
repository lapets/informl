
(function(uxadt, Informl){"use strict"; var uxadt, Informl;
  var Evaluate = {};
  if(typeof exports !== 'undefined') {if (typeof module !== 'undefined' && module.exports){exports = module.exports = Evaluate;}exports.Evaluate = Evaluate;uxadt = require('./uxadt.js');Informl = require('./Informl.js');}
  else {window.Evaluate = Evaluate;uxadt = window.uxadt; Informl = window.Informl;}
  uxadt.qualified('Test', {'Num':[null],'Plus':[null,null],'Minus':[null,null],'Mult':[null,null],'Div':[null,null],'And':[null,null],'Or':[null,null],'Not':[null],'Assign':[null,null],'Var':[null],'Print':[null],'Term':[null],'If':[null,null],'Formula':[null],'Vero':[],'Falso':[],'Program':[null],'This':[null],'That':[null,null],'Alp':[]});

  
  
  
  
  Evaluate.evalTerm = function (term, env) {
    var __iml0 = term;var __iml1 = null; __iml1 = __iml0._(Test.Num(null), function(){return true;})._(Test.Plus(null,null), function(){return true;})._(Test.Minus(null,null), function(){return true;})._(Test.Mult(null,null), function(){return true;})._(Test.Div(null,null), function(){return true;})._(Test.Var(null), function(){return true;}).end;if(__iml1 == null){
    return "wrong";}
    __iml1 = __iml0._(Test.Num(null), function (a) {
    return a;} )._(Test.Plus(null,null), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return Informl.plus(x, y);} )._(Test.Minus(null,null), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return Informl.minus(x, y);} )._(Test.Mult(null,null), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return (x * y);} )._(Test.Div(null,null), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return (x / y);} )._(Test.Var(null), function (a) {
    var __iml2 = env[a];var __iml3 = null; __iml3 = __iml2._(Test.Term(null), function(){return true;}).end;if(__iml3 == null){throw new Error('Pattern did not match');}
    __iml3 = __iml2._(Test.Term(null), function (t) {
    return Evaluate.evalTerm(t, env);} ).end;
    return __iml3;} ).end;
    return __iml1;
  }
  
  Evaluate.evalForm = function (form, env) {
    var __iml4 = form;var __iml5 = null; __iml5 = __iml4._(Test.Vero(), function(){return true;})._(Test.Falso(), function(){return true;})._(Test.And(null,null), function(){return true;})._(Test.Or(null,null), function(){return true;})._(Test.Not(null), function(){return true;})._(Test.Var(null), function(){return true;}).end;if(__iml5 == null){throw new Error('Pattern did not match');}
    __iml5 = __iml4._(Test.Vero(), function () {
    return Test.Vero();} )._(Test.Falso(), function () {
    return Test.Falso();} )._(Test.And(null,null), function (a,b) {
    var __iml6 = Evaluate.evalForm(a, env);var __iml7 = null; __iml7 = __iml6._(Test.Falso(), function(){return true;})._(Test.Vero(), function(){return true;}).end;if(__iml7 == null){throw new Error('Pattern did not match');}
    __iml7 = __iml6._(Test.Falso(), function () {
    return Test.Falso();} )._(Test.Vero(), function () {
    return Evaluate.evalForm(b, env);} ).end;
    return __iml7;} )._(Test.Or(null,null), function (a,b) {
    var __iml8 = Evaluate.evalForm(a, env);var __iml9 = null; __iml9 = __iml8._(Test.Vero(), function(){return true;})._(Test.Falso(), function(){return true;}).end;if(__iml9 == null){throw new Error('Pattern did not match');}
    __iml9 = __iml8._(Test.Vero(), function () {
    return Test.Vero();} )._(Test.Falso(), function () {
    return Evaluate.evalForm(b, env);} ).end;
    return __iml9;} )._(Test.Not(null), function (a) {
    var __iml10 = Evaluate.evalForm(a, env);var __iml11 = null; __iml11 = __iml10._(Test.Vero(), function(){return true;})._(Test.Falso(), function(){return true;}).end;if(__iml11 == null){throw new Error('Pattern did not match');}
    __iml11 = __iml10._(Test.Vero(), function () {
    return Test.Falso();} )._(Test.Falso(), function () {
    return Test.Vero();} ).end;
    return __iml11;} )._(Test.Var(null), function (a) {
    var __iml12 = env[a];var __iml13 = null; __iml13 = __iml12._(Test.Formula(null), function(){return true;}).end;if(__iml13 == null){throw new Error('Pattern did not match');}
    __iml13 = __iml12._(Test.Formula(null), function (f) {
    return Evaluate.evalForm(f, env);} ).end;
    return __iml13;} ).end;
    return __iml5;
  }
  
  Evaluate.evalStatement = function (stmt, env) {
    var __iml14 = stmt;var __iml15 = null; __iml15 = __iml14._(Test.Assign(null,null), function(){return true;})._(Test.Print(null), function(){return true;})._(Test.If(Test.Formula(null),null), function(){return true;}).end;if(__iml15 == null){throw new Error('Pattern did not match');}
    __iml15 = __iml14._(Test.Assign(null,null), function (v,val) {
    var __iml16 = v;var __iml17 = null; __iml17 = __iml16._(Test.Var(null), function(){return true;}).end;if(__iml17 == null){throw new Error('Pattern did not match');}
    __iml17 = __iml16._(Test.Var(null), function (a) {
    env[a] = val;
    return null;} ).end;
    return __iml17;} )._(Test.Print(null), function (o) {
    var __iml18 = o;var __iml19 = null; __iml19 = __iml18._(Test.Formula(null), function(){return true;})._(Test.Term(null), function(){return true;}).end;if(__iml19 == null){throw new Error('Pattern did not match');}
    __iml19 = __iml18._(Test.Formula(null), function (f) {
    return Evaluate.evalForm(f, env);} )._(Test.Term(null), function (t) {
    return Evaluate.evalTerm(t, env);} ).end;
    return __iml19;} )._(Test.If(Test.Formula(null),null), function (e,block) {
    var __iml20 = Evaluate.evalForm(e, env);var __iml21 = null; __iml21 = __iml20._(Test.Vero(), function(){return true;})._(Test.Falso(), function(){return true;}).end;if(__iml21 == null){throw new Error('Pattern did not match');}
    __iml21 = __iml20._(Test.Vero(), function () {
    return Evaluate.evalStatement(block, env);} )._(Test.Falso(), function () {
    return null;} ).end;
    return __iml21;} ).end;
    return __iml15;
  }
  
  Evaluate.execute = function (prog) {
    var env = {};
    var __iml22 = prog;var __iml23 = null; __iml23 = __iml22._(Test.Program(null), function(){return true;}).end;if(__iml23 == null){throw new Error('Pattern did not match');}
    __iml23 = __iml22._(Test.Program(null), function (ss) {
    var out = Informl.map(ss, function(x){return Evaluate.evalStatement(x, env)});
    var ret = [];
    var __iml26 = false;var __iml24 = out; if(Informl.type(__iml24)==='pattern') { __iml24 = Informl.unpack(__iml24); __iml26 = true;}
     if(Informl.type(__iml24)==='list') { __iml24 = Informl.listToDict(__iml24); __iml26 = true;}for (var __iml25 in __iml24){ var o = __iml26 ? __iml24[__iml25] : __iml25;
      if (o != null) {
        ret = Informl.plus(ret, o);
      }
    }
    return ret;} ).end;
    return __iml23;
  }
  
  Evaluate.getTest = function (x) {
    var __iml27 = x;var __iml28 = null; __iml28 = __iml27._(Test.This(null), function(){return true;}).end;if(__iml28 == null){
    return "otherwise get";}
    __iml28 = __iml27._(Test.This(null), function (a) {
    return "hi";} ).end;
    return __iml28;
  }
  
  Evaluate.setTest = function (x) {
    var __iml29 = (x);var tmp = null; tmp = __iml29._(Test.That(Test.Alp(),null), function(){return true;}).end;
    if(tmp == null){tmp = "otherwise set";}
    else tmp = __iml29._(Test.That(Test.Alp(),null), function (a) {
    return "hi";} ).end;
    return tmp;
  }
  }(typeof exports !== 'undefined' ? exports : (this.Evaluate = {})));