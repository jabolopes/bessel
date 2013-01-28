me Examples.Nested

|--
def f := \x@ g
  where {
    def g := hd:x
  }
--|

def g x@ := x

def f2 := \x@ <~(g:x),g:x>

def f := \x@ <~g,g>
  where {
    def g := x
  }