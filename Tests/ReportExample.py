import uxadt
import Informl
Informl = Informl.Informl
uxadt.qualified('La', {'Program':[None],'Page':[None],'Repeat':[None],'Block':[None,None,None],'Prints':[None],'Errors':[],'Span':[None,None,None],'HighlightError':[],'Entity':[None],'Space':[],'Pass':[],'Line':[None],'OpPlus':[None,None],'Keyword':[None],'OpMax':[None,None],'OpAbs':[None],'Concat':[None],'Builtin':[None],'Punctuation':[None],'Num':[None],'Atom':[None,None,None],'Text':[None],'Konstant':[None]})
class ReportExample:
  

  
  
  def report (a):
    __iml0 = a
    __iml1 = __iml0._(La.Program(None), lambda ss: 1)._(La.Repeat(None), lambda ss: 1)._(La.Prints(None), lambda e: 1)._(La.Errors(), lambda : 1)._(La.Pass(), lambda : 1)._(La.OpPlus(None,None), lambda e1,e2: 1)._(La.OpMax(None,None), lambda e1,e2: 1)._(La.OpAbs(None), lambda e: 1)._(La.Num(None), lambda n: 1).end
    if __iml1 != 1:
      raise NameError("Pattern Missmatch")
    if __iml0 < La.Program(None):
      (ss,) = __iml0
      return La.Page(La.Block([], [], Informl.map(ss, ReportExample.report)))
    if __iml0 < La.Repeat(None):
      (ss,) = __iml0
      line = La.Line([La.Keyword("repeat")])# Local variable.
      block = La.Block([], [], Informl.map(ss, ReportExample.report))# Local variable.
      return La.Concat([line, block])
    if __iml0 < La.Prints(None):
      (e,) = __iml0
      return La.Line([La.Keyword("print"), La.Entity(La.Space()), ReportExample.report(e)])
    if __iml0 < La.Errors():
    #  (,) = __iml0
      msg = "This is a message that applies to the whole highlighted span."# Local variable.
      span = La.Span([La.HighlightError()], [La.Text(msg)], [La.Keyword("error")])# Local variable.
      return La.Line([span, La.Entity(La.Space()), La.Text("outside"), La.Entity(La.Space()), La.Text("span")])
    if __iml0 < La.Pass():
    #  (,) = __iml0
      return La.Line([La.Keyword("pass")])
    if __iml0 < La.OpPlus(None,None):
      (e1,e2,) = __iml0
      return La.Concat([ReportExample.report(e1), La.Keyword("+"), ReportExample.report(e2)])
    if __iml0 < La.OpMax(None,None):
      (e1,e2,) = __iml0
      return La.Concat([La.Builtin("max"), La.Punctuation("("), ReportExample.report(e1), La.Punctuation(","), ReportExample.report(e2), La.Punctuation(")")])
    if __iml0 < La.OpAbs(None):
      (e,) = __iml0
      return La.Concat([La.Builtin("abs"), La.Punctuation("("), ReportExample.report(e), La.Punctuation(")")])
    if __iml0 < La.Num(None):
      (n,) = __iml0
      return La.Atom([], [La.Text("int")], [La.Konstant(Informl.plus(n, ""))])
    
    
  
