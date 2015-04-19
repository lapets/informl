
var ImparseToInforml = (function(uxadt, Informl){var ImparseToInforml = {};
  uxadt.qualified('Type', {'Grammar':[null],'Production':[null,null],'Choices':[null],'Choice':[null,null,null],'Terminal':[null],'Nonterminal':[null]});
  uxadt.qualified('ImparseToInforml_ERROR',{PatternMismatch:[]});
  
  
  
  ImparseToInforml.fresh = 0;
  
  ImparseToInforml.evalGrammar = function (g) {
    var __iml0; try {__iml0 = (g)._(Type.Grammar(null), function (prodlist) {
    var comp = "module MyLang\n  where\n  all are qualified Iml\n\n";
    var __iml3 = false;var __iml1 = prodlist; if(Informl.type(__iml1)==='pattern') { obj = Informl.unpack(__iml1); __iml3 = true;}
     if(Informl.type(__iml1)==='list') { obj = Informl.listToDict(__iml1); __iml3 = true;}for (var __iml2 in __iml1){ var prod = __iml3 ? __iml1[__iml2] : __iml2;
      comp = Informl.plus(comp, ImparseToInforml.evalProduction(a));
    }
    return comp;} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml0 == 'undefined') return ImparseToInforml_ERROR.PatternMismatch(); return __iml0;
  }
  
  ImparseToInforml.evalProduction = function (production) {
    var __iml4; try {__iml4 = (production)._(Type.Production(null,null), function (prod,choicesList) {
    var __iml7 = false;var __iml5 = choicesList; if(Informl.type(__iml5)==='pattern') { obj = Informl.unpack(__iml5); __iml7 = true;}
     if(Informl.type(__iml5)==='list') { obj = Informl.listToDict(__iml5); __iml7 = true;}for (var __iml6 in __iml5){ var choices = __iml7 ? __iml5[__iml6] : __iml6;
      var __iml8; try {__iml8 = (choices)._(Type.Choices(null), function (list) {
      var comp = "";
      var __iml11 = false;var __iml9 = list; if(Informl.type(__iml9)==='pattern') { obj = Informl.unpack(__iml9); __iml11 = true;}
       if(Informl.type(__iml9)==='list') { obj = Informl.listToDict(__iml9); __iml11 = true;}for (var __iml10 in __iml9){ var choice = __iml11 ? __iml9[__iml10] : __iml10;
        comp = Informl.plus(comp, ImparseToInforml.evalChoice(choice));
      }
      return Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus("  function eval", prod), "(x)\n"), comp), "\n    get"), " x\n");} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml8 == 'undefined') return ImparseToInforml_ERROR.PatternMismatch(); return __iml8;
    }} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml4 == 'undefined') return ImparseToInforml_ERROR.PatternMismatch(); return __iml4;
  }
  
  ImparseToInforml.freshWPre = function (pre) {
    var retval = Informl.plus(pre, ImparseToInforml.fresh);
    ImparseToInforml.fresh = Informl.plus(ImparseToInforml.fresh, 1);
    return retval;
  }
  
  ImparseToInforml.evalChoice = function (choice) {
    var __iml12; try {__iml12 = (choice)._(Type.Choice(null,null,null), function (a,x,b) {
    var evallist = [];
    var __iml15 = false;var __iml13 = b; if(Informl.type(__iml13)==='pattern') { obj = Informl.unpack(__iml13); __iml15 = true;}
     if(Informl.type(__iml13)==='list') { obj = Informl.listToDict(__iml13); __iml15 = true;}for (var __iml14 in __iml13){ var v = __iml15 ? __iml13[__iml14] : __iml14;
      var tmp; try {tmp = (v)._(Type.Terminal(null), function (f) {
      return [];} )._(Type.Nonterminal(null), function (f) {
      return [f];} ).end;}catch(err){
      return [""];}if(typeof tmp == 'undefined') {
      return [""];}
      evallist = Informl.plus(evallist, tmp);
    }
    var dec = [];
    var pattern = "";
    var i = 1;
    var j = 1;
    var __iml18 = false;var __iml16 = evallist; if(Informl.type(__iml16)==='pattern') { obj = Informl.unpack(__iml16); __iml18 = true;}
     if(Informl.type(__iml16)==='list') { obj = Informl.listToDict(__iml16); __iml18 = true;}for (var __iml17 in __iml16){ var e = __iml18 ? __iml16[__iml17] : __iml17;
      if (e != "") {
        var loc = Informl.plus(Informl.plus("local ", "x"), j);
        var rec = Informl.plus("y", i);
        dec = Informl.plus(dec, [rec]);
        pattern = Informl.plus(pattern, Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus("        ", loc), " = eval"), e), "("), rec), ")\n\n"));
        j = Informl.plus(j, 1);
      }
      else {
        dec = Informl.plus(dec, [Informl.plus("z", i)]);
      }
      i = Informl.plus(i, 1);
    }
    var decc = Informl.plus(Informl.plus("(", Informl.join(dec, ",")), ")\n");
    return Informl.plus(Informl.plus(Informl.plus("      ", a), decc), pattern);} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml12 == 'undefined') return ImparseToInforml_ERROR.PatternMismatch(); return __iml12;
  }
  return ImparseToInforml;
  }(uxadt, Informl));