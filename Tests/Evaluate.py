import uxadt
import Informl
import richreports as Rr
Informl = Informl.Informl
uxadt.qualified('Test', {'Num':[None],'Plus':[None,None],'Minus':[None,None],'Mult':[None,None],'Div':[None,None],'And':[None,None],'Or':[None,None],'Not':[None],'Assign':[None,None],'Var':[None],'Print':[None],'Term':[None],'If':[None,None],'Formula':[None],'Vero':[],'Falso':[],'Program':[None],'This':[None],'That':[None,None],'Alp':[]})
class Evaluate:
  

  
  
  def evalTerm (term, env):
    __iml0 = term
    __iml1 = __iml0._(Test.Num(None), lambda a: 1)._(Test.Plus(None,None), lambda a,b: 1)._(Test.Minus(None,None), lambda a,b: 1)._(Test.Mult(None,None), lambda a,b: 1)._(Test.Div(None,None), lambda a,b: 1)._(Test.Var(None), lambda a: 1).end
    if __iml1 != 1:
      return "wrong"
    if __iml0 < Test.Num(None):
      (a,) = __iml0
      return a
    if __iml0 < Test.Plus(None,None):
      (a,b,) = __iml0
      x = Evaluate.evalTerm(a, env)# Local variable.
      y = Evaluate.evalTerm(b, env)# Local variable.
      return informl.plus(x, y)
    if __iml0 < Test.Minus(None,None):
      (a,b,) = __iml0
      x = Evaluate.evalTerm(a, env)# Local variable.
      y = Evaluate.evalTerm(b, env)# Local variable.
      return (x - y)
    if __iml0 < Test.Mult(None,None):
      (a,b,) = __iml0
      x = Evaluate.evalTerm(a, env)# Local variable.
      y = Evaluate.evalTerm(b, env)# Local variable.
      return (x * y)
    if __iml0 < Test.Div(None,None):
      (a,b,) = __iml0
      x = Evaluate.evalTerm(a, env)# Local variable.
      y = Evaluate.evalTerm(b, env)# Local variable.
      return informl.div(x, y)
    if __iml0 < Test.Var(None):
      (a,) = __iml0
      __iml2 = env[a]
      __iml3 = __iml2._(Test.Term(None), lambda t: 1).end
      if __iml3 != 1:
        return None
      if __iml2 < Test.Term(None):
        (t,) = __iml2
        return Evaluate.evalTerm(t, env)
      
      
    
    
  
  
  def evalForm (form, env):
    __iml4 = form
    __iml5 = __iml4._(Test.Vero(), lambda : 1)._(Test.Falso(), lambda : 1)._(Test.And(None,None), lambda a,b: 1)._(Test.Or(None,None), lambda a,b: 1)._(Test.Not(None), lambda a: 1)._(Test.Var(None), lambda a: 1).end
    if __iml5 != 1:
      return None
    if __iml4 < Test.Vero():
    #  (,) = __iml4
      return Test.Vero()
    if __iml4 < Test.Falso():
    #  (,) = __iml4
      return Test.Falso()
    if __iml4 < Test.And(None,None):
      (a,b,) = __iml4
      __iml6 = Evaluate.evalForm(a, env)
      __iml7 = __iml6._(Test.Falso(), lambda : 1)._(Test.Vero(), lambda : 1).end
      if __iml7 != 1:
        return None
      if __iml6 < Test.Falso():
      #  (,) = __iml6
        return Test.Falso()
      if __iml6 < Test.Vero():
      #  (,) = __iml6
        return Evaluate.evalForm(b, env)
      
      
    if __iml4 < Test.Or(None,None):
      (a,b,) = __iml4
      __iml8 = Evaluate.evalForm(a, env)
      __iml9 = __iml8._(Test.Vero(), lambda : 1)._(Test.Falso(), lambda : 1).end
      if __iml9 != 1:
        return None
      if __iml8 < Test.Vero():
      #  (,) = __iml8
        return Test.Vero()
      if __iml8 < Test.Falso():
      #  (,) = __iml8
        return Evaluate.evalForm(b, env)
      
      
    if __iml4 < Test.Not(None):
      (a,) = __iml4
      __iml10 = Evaluate.evalForm(a, env)
      __iml11 = __iml10._(Test.Vero(), lambda : 1)._(Test.Falso(), lambda : 1).end
      if __iml11 != 1:
        return None
      if __iml10 < Test.Vero():
      #  (,) = __iml10
        return Test.Falso()
      if __iml10 < Test.Falso():
      #  (,) = __iml10
        return Test.Vero()
      
      
    if __iml4 < Test.Var(None):
      (a,) = __iml4
      __iml12 = env[a]
      __iml13 = __iml12._(Test.Formula(None), lambda f: 1).end
      if __iml13 != 1:
        return None
      if __iml12 < Test.Formula(None):
        (f,) = __iml12
        return Evaluate.evalForm(f, env)
      
      
    
    
  
  
  def evalStatement (stmt, env):
    __iml14 = stmt
    __iml15 = __iml14._(Test.Assign(None,None), lambda v,val: 1)._(Test.Print(None), lambda o: 1)._(Test.If(Test.Formula(None),None), lambda e,block: 1).end
    if __iml15 != 1:
      return None
    if __iml14 < Test.Assign(None,None):
      (v,val,) = __iml14
      __iml16 = v
      __iml17 = __iml16._(Test.Var(None), lambda a: 1).end
      if __iml17 != 1:
        return None
      if __iml16 < Test.Var(None):
        (a,) = __iml16
        env[a] = val
        return None
      
      
    if __iml14 < Test.Print(None):
      (o,) = __iml14
      __iml18 = o
      __iml19 = __iml18._(Test.Formula(None), lambda f: 1)._(Test.Term(None), lambda t: 1).end
      if __iml19 != 1:
        return None
      if __iml18 < Test.Formula(None):
        (f,) = __iml18
        return Evaluate.evalForm(f, env)
      if __iml18 < Test.Term(None):
        (t,) = __iml18
        return Evaluate.evalTerm(t, env)
      
      
    if __iml14 < Test.If(Test.Formula(None),None):
      (e,block,) = __iml14
      __iml20 = Evaluate.evalForm(e, env)
      __iml21 = __iml20._(Test.Vero(), lambda : 1)._(Test.Falso(), lambda : 1).end
      if __iml21 != 1:
        return None
      if __iml20 < Test.Vero():
      #  (,) = __iml20
        return Evaluate.evalStatement(block, env)
      if __iml20 < Test.Falso():
      #  (,) = __iml20
        return None
      
      
    
    
  
  
  def execute (prog):
    env = {}# Local variable.
    __iml22 = prog
    __iml23 = __iml22._(Test.Program(None), lambda ss: 1).end
    if __iml23 != 1:
      return None
    if __iml22 < Test.Program(None):
      (ss,) = __iml22
      out = Informl.map(ss, lambda x : Evaluate.evalStatement(x, env))# Local variable.
      ret = []# Local variable.
      for o in out:
        if o != None:
          ret = Informl.plus(ret, o)
        
      
      return ret
    
    
  
  
  def getTest (x):
    __iml24 = x
    __iml25 = __iml24._(Test.This(None), lambda a: 1).end
    if __iml25 != 1:
      return "otherwise get"
    if __iml24 < Test.This(None):
      (a,) = __iml24
      return "hi"
    
    
  
  
  def setTest (x):
    __iml26 = x
    tmp = __iml26._(Test.That(Test.Alp(),None), lambda a: 1).end
    __iml_enterd27 = True
    if tmp != 1:
      __iml_enterd27 = False
      tmp = "otherwise set"
    if __iml_enterd27:
      if __iml26 < Test.That(Test.Alp(),None):
        (a,) = __iml26
        tmp = "hi"
      
      "ENDOFSET"
    
    return tmp
  
