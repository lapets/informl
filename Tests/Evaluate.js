var _ = null;
uxadt.qualified('Bool', {'True':[],'False':[]});
uxadt.qualified('La', {'Plus':[_,_],'Minus':[_,_],'Mult':[_,_],'Div':[_,_],'And':[_,_],'Or':[_,_],'Var':[_],'Assign':[_,_,_],'Print':[_,_],'Term':[_],'If':[_,_,_],'True':[],'False':[],'End':[],'Num':[_],'Just':[_],'Nothing':[],'Formula':[_],'Not':[_]});
uxadt.qualified('Evaluate_ERROR',{PatternMismatch:[]});
var Evaluate = (function(uxadt, Informl){var Evaluate = {};
  
  
  
  
  Evaluate.findInEnv = function (v, env) {
    var __iml0 = env;for (var __iml1 = 0; __iml1 < __iml0.length; __iml1++) {var item = __iml0[__iml1];
      if (item[0] == v) {
        return val;
      }
    }
  }
  
  Evaluate.evalTerm = function (term, env) {
    var __iml2; try {__iml2 = (term)._(La.Num(_), function (a) {
    return a;} )._(La.Plus(_,_), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return Informl.plus(x, y);} )._(La.Minus(_,_), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return Informl.plus(x, y);} )._(La.Mult(_,_), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return (x * y);} )._(La.Div(_,_), function (a,b) {
    var x = Evaluate.evalTerm(a, env);
    var y = Evaluate.evalTerm(b, env);
    return Informl.div(x, y);} )._(La.Var(_), function (a) {
    return Evaluate.evalTerm(Evaluate.findInEnv(a));} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml2 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml2;
  }
  
  Evaluate.evalForm = function (form, env) {
    var __iml3; try {__iml3 = (form)._(Bool.True(), function () {
    return Bool.True();} )._(Bool.False(), function () {
    return Bool.False();} )._(La.And(_,_), function (a,b) {
    var __iml4; try {__iml4 = (Evaluate.evalForm(a, env))._(Bool.False(), function () {
    return Bool.False();} )._(Bool.True(), function () {
    return Evaluate.evalForm(b, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml4 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml4;} )._(La.Or(_,_), function (a,b) {
    var __iml5; try {__iml5 = (Evaluate.evalForm(a, env))._(Bool.True(), function () {
    return Bool.True();} )._(Bool.False(), function () {
    return Evaluate.evalForm(b, env);} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml5 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml5;} )._(La.Not(_), function (a) {
    var __iml6; try {__iml6 = (Evaluate.evalForm(a, env))._(Bool.True(), function () {
    return Bool.False();} )._(Bool.False(), function () {
    return Bool.True();} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml6 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml6;} )._(La.Var(_), function (a) {
    return Evaluate.evalTerm(Evaluate.findInEnv(a));} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml3 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml3;
  }
  
  Evaluate.execute = function (prog, env) {
    var __iml7; try {__iml7 = (prog)._(La.Assign(_,_,_), function (v,val,rest) {
    return Evaluate.execute(rest, Informl.plus([[v, val]], env));} )._(La.Print(_,_), function (o,rest) {
    var __iml8; try {__iml8 = (o)._(La.Formula(_), function (f) {
    return Informl.plus([Evaluate.evalForm(f, env)], Evaluate.execute(rest, env));} )._(La.Term(_), function (t) {
    return Informl.plus([Evaluate.evalTerm(t, env)], Evaluate.execute(rest, env));} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml8 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml8;} )._(La.If(La.Formula(_),_,_), function (e,block,rest) {
    var result; try {result = (Evaluate.evalForm(e, env))._(Bool.True(), function () {
    return Evaluate.execute(block, env);} )._(Bool.False(), function () {
    return [];} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof result == 'undefined') return Evaluate_ERROR.PatternMismatch();
    return Informl.plus(result, Evaluate.execute(rest, env));} )._(La.End(), function () {
    return [];} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof __iml7 == 'undefined') return Evaluate_ERROR.PatternMismatch(); return __iml7;
  }
  
  Evaluate.maybeNumTimes2 = function (n) {
    var __iml9; try {__iml9 = (n)._(La.Num(_), function (a) {
    return La.Just((a * 2));} ).end;}catch(err){
    return La.Nothing();}if(typeof __iml9 == 'undefined') {
    return La.Nothing();} return __iml9;
  }
  
  Evaluate.maybeNot = function (f) {
    var form; try {form = (f)._(La.Formula(_), function (a) {
    return a;} ).end;}catch(err){return Evaluate_ERROR.PatternMismatch();} if(typeof form == 'undefined') return Evaluate_ERROR.PatternMismatch();
    return Evaluate.evalForm(La.Not(form));
  }
  return Evaluate;
  }(uxadt, Informl));