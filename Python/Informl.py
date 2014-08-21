#Informl library

import uxadt
import builtins

def size(o):
    if o is None:
      return None
    if builtins.type(o) is list or builtins.type(o) is str:
      return len(o)
    return None

def plus(o1,o2):
    if o1 is None or o2 is None:
      return None
    if builtins.type(o1) is list and builtins.type(o2) is list:
      return o1 + o2
    if builtins.type(o1) is list:
      return o1 + [o2]
    if builtins.type(o2) is list:
      return [o1] + o2
    if builtins.type(o1) is str:
      return o1 + str(o2)
    if builtins.type(o2) is str:
      return str(o1) + o2
    return o1 + o2

def minus(o1,o2):
    if o1 is None or o2 is None:
      return None
    if builtins.type(o1) is list and builtins.type(o2) is list:
      return [x for x in o1 if not x in o2]
    if builtins.type(o1) is list:
      return [x for x in o1 if x != o2]
    if builtins.type(o2) is list:
      return [x for x in o2 if x != o1]
    return o1 - o2

def map(o,f):
    return [f(x) for x in o]

def fold(o,f,b):
    return b if (len(o) == 0) else fold(o[:-1], f ,f(o[len(o)-1], b))

def type(o):
    if builtins.type(o) is list:
      return "list"
    if builtins.type(o) is str:
      return "string"
    if builtins.type(o) is float or builtins.type(o) is int:
      return "number"
    if builtins.type(o) is dict:
      return "dictionary"
    if isinstance(o,uxadt.Value):
      return "pattern"
    if callable(o):
      return "function"
    return "null";

def range(x,y):
    return builtins.range(x,y)

