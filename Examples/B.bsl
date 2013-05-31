me Examples.B

use Examples.A

module M1 where {
  def b1 = a
}

module M2 where {
  use Examples.C

  def b2 = f
}

def b3 = a
def b4 = M1.b1
def b5 = M2.b2