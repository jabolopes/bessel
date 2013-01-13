me Examples.List

def c0 := x@ -> x ; x
def c1 := x@ -> x
def c2 <- x@ := x

def d1 := [~2,id]

def f0 := x@ -> add ; sub
def f1 := isint -> add ; sub
def f2 := x@isint -> add ; sub

def f3 := [|x@|] -> add ; sub
def f4 := [|isint|] -> add ; sub
def f5 := [|x@isint|] -> add ; sub

def f6 := x@[|y@|] -> add ; sub
def f7 := x@[|isint|] -> add ; sub
def f8 := x@[|y@isint|] -> add ; sub

def f9 := a@[|y@,isnum|] -> add ; sub
def f10 := [|tt,tt,tt|] -> add ; sub
def f11 := a@[|x@isint,y@,isnum|] -> add ; sub
def f12 := isint -> g ; h where { def g := add def h := sub }
def f13 := x@[|a@isint,b@isnum|] -> g ; h where { def g := add def h := sub }

-- sig j := tt =f=> tt
-- def j := ~"ola"

-- sig l :: seqof:isnum =f=> isnum
-- sig l :: seqof:isint =f=> isint
-- sig l :: seqof:isreal =f=> isreal
-- sig l :: seqof:isnum =f=> seqof:isnum
-- def l := comp:<sub,add>

def g1 := cons
def g2 := [add,sub]
def g3 := [sub,add]:<1,2>
def g4 := [add,sub]:<1.2,2.1>
def g5 := [sub,add]:<1,2.1>

def a0 := ~1
def a1 := ~1.2
def a2 := ~<1,2.1,3>
def a3 := ~(add:<1,2,3>)
def a4 := ~(add:<1,2.1,3>)

def k := out o [~"scr",id]

def h0 <- x@ := add
def h1 <- isint := add
def h2 <- x@isreal := add
def h3 <- [|x@|] := add
def h4 <- [|tt,tt,tt|] := sub
def h5 <- a@[|x@isint,y@,isnum|] := sub
def h6 <- isint := g where { def g := add }
def h7 <- x@[|a@isint,b@isnum|] := h where { def h := sub }

def l0 := \x@ x
def l1 := \x@isint x
def l2 := \x@isreal x
def l3 := \[|x@isint|] x
def l4 := \[|[|y@isint|]|] y
def l5 := \[|x@isint,y@isreal|] x
def l6 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] a
def l7 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] x
def l8 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] y
def l9 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] z
def l10 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] u
def l11 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] v
def l12 := \a@[|x@,y@isint,z@isnum,isreal,isnum,u@[|v@isbool,w@isbool|]|] w

def s0 := s:1:<2>

def o1 := o:<sub,add>
def o2 := o:<sub,add>:<1,2>

def init := [|x@|] -> ~<> ; al o [hd,init o tl]
def last := [|x@|] -> x ; last o tl