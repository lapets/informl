
var ImparseToInforml = (function(uxadt, Informl){var ImparseToInforml = {};
  uxadt.qualified('Type', {'Grammar':[null],'Production':[null,null],'Choices':[null],'Choice':[null,null,null],'AssocNone':[],'Terminal':[null],'Nonterminal':[null],'RegExpr':[null],'Many':[null],'May':[null],'MayMany':[null],'One':[null]});
  uxadt.qualified('ImparseToInforml_ERROR',{PatternMismatch:[]});
  
  
  
  ImparseToInforml.manyCons = [];
  
  ImparseToInforml.evalGrammar = function (g) {
    ImparseToInforml.manyCons = [];
    var __iml0 = '_@_flag1'; try {__iml0 = (g)._(Type.Grammar(null), function (prodlist) {
    var comp = "module MyLang\n  where\n    all are qualified Iml\n\n";
    comp = Informl.plus(comp, "  function execute(prog)\n    local env = {\"output\":[]}\n\n");
    comp = Informl.plus(comp, "    return env[\"output\"]\n\n");
    var __iml4 = false;var __iml2 = prodlist; if(Informl.type(__iml2)==='pattern') { __iml2 = Informl.unpack(__iml2); __iml4 = true;}
     if(Informl.type(__iml2)==='list') { __iml2 = Informl.listToDict(__iml2); __iml4 = true;}for (var __iml3 in __iml2){ var prod = __iml4 ? __iml2[__iml3] : __iml3;
      comp = Informl.plus(comp, ImparseToInforml.evalProduction(prod));
    }
    if (Informl.type(comp) == "list") {
      comp = Informl.join(comp, "");
    }
    return comp;} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml0 == '_@_flag1')return ImparseToInforml_ERROR.PatternMismatch(); return __iml0;
  }
  
  ImparseToInforml.evalProduction = function (production) {
    var __iml5 = '_@_flag6'; try {__iml5 = (production)._(Type.Production(null,null), function (prod,choicesList) {
    var __iml9 = false;var __iml7 = choicesList; if(Informl.type(__iml7)==='pattern') { __iml7 = Informl.unpack(__iml7); __iml9 = true;}
     if(Informl.type(__iml7)==='list') { __iml7 = Informl.listToDict(__iml7); __iml9 = true;}for (var __iml8 in __iml7){ var choices = __iml9 ? __iml7[__iml8] : __iml8;
      var retval = '_@_flag10'; try {retval = (choices)._(Type.Choices(null), function (list) {
      var comp = "";
      var __iml13 = false;var __iml11 = list; if(Informl.type(__iml11)==='pattern') { __iml11 = Informl.unpack(__iml11); __iml13 = true;}
       if(Informl.type(__iml11)==='list') { __iml11 = Informl.listToDict(__iml11); __iml13 = true;}for (var __iml12 in __iml11){ var choice = __iml13 ? __iml11[__iml12] : __iml12;
        comp = Informl.plus(comp, ImparseToInforml.evalChoice(choice));
      }
      return Informl.plus(Informl.plus(Informl.plus("  function eval", prod), "(x,env)\n    get x\n"), comp);} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof retval == '_@_flag10') return ImparseToInforml_ERROR.PatternMismatch();
      return retval;
    }} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml5 == '_@_flag6')return ImparseToInforml_ERROR.PatternMismatch(); return __iml5;
  }
  
  ImparseToInforml.evalChoice = function (choice) {
    var __iml14 = '_@_flag15'; try {__iml14 = (choice)._(Type.Choice(null,Type.AssocNone(),null), function (a,b) {
    var dec = [];
    var pattern = "";
    var i = 1;
    var j = 1;
    var decc = "\n\n";
    
    var extract = function (t) {
      var __iml16 = '_@_flag17'; try {__iml16 = (t)._(Type.Terminal(null), function (f) {
      return [];} )._(Type.Nonterminal(null), function (f) {
      return [f];} )._(Type.RegExpr(null), function (f) {
      return [""];} )._(Type.Many(null), function (l) {
      ImparseToInforml.manyCons = Informl.plus(ImparseToInforml.manyCons, a);
      return [Informl.fold(Informl.map(l, extract), function(x,y){return Informl.plus(x, y)}, [])];} )._(Type.May(null), function (l) {
      ImparseToInforml.manyCons = Informl.plus(ImparseToInforml.manyCons, a);
      return [Informl.fold(Informl.map(l, extract), function(x,y){return Informl.plus(x, y)}, [])];} )._(Type.MayMany(null), function (l) {
      ImparseToInforml.manyCons = Informl.plus(ImparseToInforml.manyCons, a);
      return [Informl.fold(Informl.map(l, extract), function(x,y){return Informl.plus(x, y)}, [])];} )._(Type.One(null), function (l) {
      ImparseToInforml.manyCons = Informl.plus(ImparseToInforml.manyCons, a);
      return [Informl.fold(Informl.map(l, extract), function(x,y){return Informl.plus(x, y)}, [])];} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml16 == '_@_flag17')return ImparseToInforml_ERROR.PatternMismatch(); return __iml16;
    }
    var evallist = Informl.fold(Informl.map(b, extract), function(x,y){return Informl.plus(x, y)}, []);
    var __iml20 = false;var __iml18 = evallist; if(Informl.type(__iml18)==='pattern') { __iml18 = Informl.unpack(__iml18); __iml20 = true;}
     if(Informl.type(__iml18)==='list') { __iml18 = Informl.listToDict(__iml18); __iml20 = true;}for (var __iml19 in __iml18){ var e = __iml20 ? __iml18[__iml19] : __iml19;
      var loc = Informl.plus("local x", j);
      var rec = Informl.plus("y", i);
      if (Informl.type(e) == "list") {
        dec = Informl.plus(dec, [Informl.plus("m", i)]);
        if ((Informl.size(e) == 1 && Informl.type(e[0]) == "string")) {
          pattern = Informl.plus(pattern, Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus("        ", loc), " = (\\w->eval"), e[0]), "(w,env)) maps m"), i));
        }
        pattern = Informl.plus(pattern, "\n\n");
      }
      else if (e != "") {
        dec = Informl.plus(dec, [rec]);
        pattern = Informl.plus(pattern, Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus(Informl.plus("        ", loc), " = eval"), e), "("), rec), ",env)\n\n"));
        j = Informl.plus(j, 1);
      }
      else {
        dec = Informl.plus(dec, [Informl.plus("z", i)]);
      }
      i = Informl.plus(i, 1);
    }
    if (Informl.size(dec) > 0) {
      decc = Informl.plus(Informl.plus("(", Informl.join(dec, ",")), ")\n\n");
    }
    return Informl.plus(Informl.plus(Informl.plus("      ", a), decc), pattern);} ).end;}catch(err){return ImparseToInforml_ERROR.PatternMismatch();} if(typeof __iml14 == '_@_flag15')return ImparseToInforml_ERROR.PatternMismatch(); return __iml14;
  }
  return ImparseToInforml;
  }(uxadt, Informl));