#class for constructing

class Informl:

  def __init__(self,field_list = []):
    self.informl_fields = field_list

  def size(self,o):
    if o is None:
      return None
    if type(o) is list or type(o) is str:
      return len(o)
    return None

  def plus(self,o1,o2):
    if o1 is None or o2 is None:
      return None
    return o1 + o2


        