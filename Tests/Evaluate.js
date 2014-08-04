
var Evaluate = (function(uxadt, Informl){var Evaluate = {};
  uxadt.qualified('Test', {'Num':[null],'Plus':[null,null],'Minus':[null,null],'Mult':[null,null],'Div':[null,null],'And':[null,null],'Or':[null,null],'Not':[null],'Assign':[null,null],'Var':[null],'Print':[null],'Term':[null],'If':[null,null],'Formula':[null],'True':[],'False':[],'Program':[null]});
  uxadt.qualified('Evaluate_ERROR',{PatternMismatch:[]});
  
  
  
  
  Evaluate.evalTerm = function (term, env) {
    var __iml0 = '_@_flag1'; try {__iml0 = (term)._(Test.Num(null), function (a) {
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
    var __iml2 = '_@_flag3'; try {__iml2 = (env[a])._(Test.Term(null), function (t) {
    return Evaluate.evalTerm(t, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml2 == '_@_flag3')return Evaluate_ERROR.PatternMismatch(); return __iml2;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml0 == '_@_flag1')return Evaluate_ERROR.PatternMismatch(); return __iml0;
  }
  
  Evaluate.evalForm = function (form, env) {
    var __iml4 = '_@_flag5'; try {__iml4 = (form)._(Test.True(), function () {
    return Test.True();} )._(Test.False(), function () {
    return Test.False();} )._(Test.And(null,null), function (a,b) {
    var __iml6 = '_@_flag7'; try {__iml6 = (Evaluate.evalForm(a, env))._(Test.False(), function () {
    return Test.False();} )._(Test.True(), function () {
    return Evaluate.evalForm(b, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml6 == '_@_flag7')return Evaluate_ERROR.PatternMismatch(); return __iml6;} )._(Test.Or(null,null), function (a,b) {
    var __iml8 = '_@_flag9'; try {__iml8 = (Evaluate.evalForm(a, env))._(Test.True(), function () {
    return Test.True();} )._(Test.False(), function () {
    return Evaluate.evalForm(b, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml8 == '_@_flag9')return Evaluate_ERROR.PatternMismatch(); return __iml8;} )._(Test.Not(null), function (a) {
    var __iml10 = '_@_flag11'; try {__iml10 = (Evaluate.evalForm(a, env))._(Test.True(), function () {
    return Test.False();} )._(Test.False(), function () {
    return Test.True();} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml10 == '_@_flag11')return Evaluate_ERROR.PatternMismatch(); return __iml10;} )._(Test.Var(null), function (a) {
    var __iml12 = '_@_flag13'; try {__iml12 = (env[a])._(Test.Formula(null), function (f) {
    return Evaluate.evalForm(f, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml12 == '_@_flag13')return Evaluate_ERROR.PatternMismatch(); return __iml12;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml4 == '_@_flag5')return Evaluate_ERROR.PatternMismatch(); return __iml4;
  }
  
  Evaluate.evalStatement = function (stmt, env) {
    var __iml14 = '_@_flag15'; try {__iml14 = (stmt)._(Test.Assign(null,null), function (v,val) {
    var __iml16 = '_@_flag17'; try {__iml16 = (v)._(Test.Var(null), function (a) {
    env[a] = val;
    return null;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml16 == '_@_flag17')return Evaluate_ERROR.PatternMismatch(); return __iml16;} )._(Test.Print(null), function (o) {
    var __iml18 = '_@_flag19'; try {__iml18 = (o)._(Test.Formula(null), function (f) {
    return Evaluate.evalForm(f, env);} )._(Test.Term(null), function (t) {
    return Evaluate.evalTerm(t, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml18 == '_@_flag19')return Evaluate_ERROR.PatternMismatch(); return __iml18;} )._(Test.If(Test.Formula(null),null), function (e,block) {
    var __iml20 = '_@_flag21'; try {__iml20 = (Evaluate.evalForm(e, env))._(Test.True(), function () {
    return Evaluate.evalStatement(block, env);} )._(Test.False(), function () {
    return null;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml20 == '_@_flag21')return Evaluate_ERROR.PatternMismatch(); return __iml20;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml14 == '_@_flag15')return Evaluate_ERROR.PatternMismatch(); return __iml14;
  }
  
  Evaluate.execute = function (prog) {
    var env = {};
    var __iml22 = '_@_flag23'; try {__iml22 = (prog)._(Test.Program(null), function (ss) {
    var out = Informl.map(ss, function(x){return Evaluate.evalStatement(x, env)});
    var ret = [];
    var __iml26 = false;var __iml24 = out; if(Informl.type(__iml24)==='pattern') { __iml24 = Informl.unpack(__iml24); __iml26 = true;}
     if(Informl.type(__iml24)==='list') { __iml24 = Informl.listToDict(__iml24); __iml26 = true;}for (var __iml25 in __iml24){ var o = __iml26 ? __iml24[__iml25] : __iml25;
      if (o != null) {
        ret = Informl.plus(ret, o);
      }
    }
    return ret;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml22 == '_@_flag23')return Evaluate_ERROR.PatternMismatch(); return __iml22;
  }
  return Evaluate;
  }(uxadt, Informl));