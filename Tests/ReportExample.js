
var ReportExample = (function(uxadt, Informl){var ReportExample = {};
    uxadt.qualified('ReportExample_ERROR',{PatternMismatch:[]});
  uxadt._({'Program':[null],'Page':[null],'Repeat':[null],'Block':[null,null,null],'Prints':[null],'Errors':[],'Span':[null,null,null],'HighlightError':[],'Entity':[null],'Space':[],'Pass':[],'Line':[null],'OpPlus':[null,null],'Keyword':[null],'OpMax':[null,null],'OpAbs':[null],'Concat':[null],'Builtin':[null],'Punctuation':[null],'Num':[null],'Atom':[null,null,null],'Text':[null],'Konstant':[null]});

  
  
  ReportExample.report = function (a) {
    var __iml0 = a;var __iml1 = null; __iml1 = __iml0._(Program(null), function(){return true;})._(Repeat(null), function(){return true;})._(Prints(null), function(){return true;})._(Errors(), function(){return true;})._(Pass(), function(){return true;})._(OpPlus(null,null), function(){return true;})._(OpMax(null,null), function(){return true;})._(OpAbs(null), function(){return true;})._(Num(null), function(){return true;}).end;if(__iml1 == null){throw new Error('Pattern did not match');}
    __iml1 = __iml0._(Program(null), function (ss) {
    return Page(Block([], [], Informl.map(ss, ReportExample.report)));} )._(Repeat(null), function (ss) {
    var line = Line([Keyword("repeat")]);
    var block = Block([], [], Informl.map(ss, ReportExample.report));
    return Concat([line, block]);} )._(Prints(null), function (e) {
    return Line([Keyword("print"), Entity(Space()), ReportExample.report(e)]);} )._(Errors(), function () {
    var msg = "This is a message that applies to the whole highlighted span.";
    var span = Span([HighlightError()], [Text(msg)], [Keyword("error")]);
    return Line([span, Entity(Space()), Text("outside"), Entity(Space()), Text("span")]);} )._(Pass(), function () {
    return Line([Keyword("pass")]);} )._(OpPlus(null,null), function (e1,e2) {
    return Concat([ReportExample.report(e1), Keyword("+"), ReportExample.report(e2)]);} )._(OpMax(null,null), function (e1,e2) {
    return Concat([Builtin("max"), Punctuation("("), ReportExample.report(e1), Punctuation(","), ReportExample.report(e2), Punctuation(")")]);} )._(OpAbs(null), function (e) {
    return Concat([Builtin("abs"), Punctuation("("), ReportExample.report(e), Punctuation(")")]);} )._(Num(null), function (n) {
    return Atom([], [Text("int")], [Konstant(Informl.plus(n, ""))]);} ).end;
    return __iml1;
  }
  return ReportExample;
  }(uxadt, Informl));