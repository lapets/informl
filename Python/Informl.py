#class for constructing

import uxadt

class Informl:

  def size(o):
    if o is None:
      return None
    if type(o) is list or type(o) is str:
      return len(o)
    return None

  def plus(o1,o2):
    if o1 is None or o2 is None:
      return None
    if type(o1) is list and type(o2) is list:
      return o1 + o2
    if type(o1) is list:
      return o1 + [o2]
    if type(o2) is list:
      return [o1] + o2
    if type(o1) is str:
      return o1 + str(o2)
    if type(o2) is str:
      return str(o1) + o2
    return o1 + o2

  def minus(o1,o2):
    if o1 is None or o2 is None:
      return None
    if type(o1) is list and type(o2) is list:
      return [x for x in o1 if not x in o2]
    if type(o1) is list:
      return [x for x in o1 if x != o2]
    if type(o2) is list:
      return [x for x in o2 if x != o1]
    return o1 - o2

  def map(o,f):
    return [f(x) for x in o]

  def fold(o,f,b):
    acc = b
    i = len(o)
    while(i >= 0):
      acc = f(o[i],acc)
    return acc

  def type(o):
    if type(o) is list:
      return "list"
    if type(o) is str:
      return "string"
    if type(o) is float or type(o) is int:
      return "number"
    if type(o) is dict:
      return "dictionary"
    if isinstance(o,uxadt.Value):
      return "pattern"
    if callable(o):
      return "function"

  def range(x,y):
    return range(x,y)