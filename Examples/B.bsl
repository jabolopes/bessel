me Examples.B

use Examples.A

module where {
  def f := 1
}

module M1 where {
  def g := 10
}

module M2 where {
  def h := f
}

module M3 where {
  def i := 10
}

module M4 where {
  def m1 := 1
  def m2 := 2
  def m3 := 3
  def m4 := 4
}

def b1 := a
def b2 := M1.g
def b3 := M2.h
def b4 := M3.i