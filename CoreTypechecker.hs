module CoreTypechecker where

import Prelude hiding (id)

import Data.Map (Map)
import qualified Data.Map as Map

import Core hiding (syms, srcfile)
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


syms :: [(String, (Type, Expr))]
syms = [
 --  -- constants
 --  ("false", BoolT, false),
 --  ("true", BoolT, true),
 --  -- predicates
 --  --- predicates for numbers
  ("isint", (predT, m isint)),
 --  ("isreal", predT, m isreal),
 --  ("isnum", predT, m isnum),
 --  ("ispos", predT, m ispos),
 --  ("isneg", predT, m isneg),
 --  ("iszero", predT, m iszero),
 --  --- predicates for other atoms, functions, and user types
 --  ("isatom", predT, m isatom),
 --  ("isbool", predT, m isbool),
 --  ("ischar", predT, m ischar),
 --  ("isutype", predT, m isutype),
 --  ("isfunc", predT, m isfunc),
 --  --- predicates for sequences
 --  ("isnull", predT, m isnull),
 --  ("ispair", predT, m ispair),
 --  ("isseq", predT, m isseq),
 --  ("isstring", predT, m isstring),
 --  --- identically true and false predicates
 --  ("ff", predT, m ff),
 --  ("isval", predT, m isval),
 --  ("tt", predT, m tt),
 --  -- boolean and comparison functions
 --  --- boolean functions
 --  ("and", ArrowT (SeqT BoolT) BoolT, m and),
 --  ("not", ArrowT BoolT BoolT, m not),
 --  ("or", ArrowT (SeqT BoolT) BoolT, m or),
 --  --- comparison functions
 --  ("eq", predTs, m eq),
 --  ("less", predTs, m less),
 --  -- arithmetic functions
 --  ("add", ArrowT (SeqT DynT) DynT, m add),
 --  ("sub", ArrowT (SeqT DynT) DynT, m sub),
 --  ("mult", ArrowT (SeqT DynT) DynT, m mul),
 --  ("div", ArrowT (SeqT DynT) DynT, m div),
 --  ("floor", ArrowT DynT DynT, m floor),
 --  ("ceiling", ArrowT DynT DynT, m ceiling),
 --  ("abs", ArrowT DynT DynT, m abs),
 --  -- combining forms
 --  ("o", ArrowT (SeqT (ArrowT DynT DynT)) (ArrowT DynT DynT), m o),
 --  ("cons", (ForallT "a" (ArrowT (SeqT (ArrowT (TvarT "a") DynT)) (ArrowT (TvarT "a") DynT))), m cons),
 --  -- ("cons", (ForallT "a" (ForallT "b" (ArrowT (SeqT (ArrowT (TvarT "a") (TvarT "b"))) (ArrowT (TvarT "a") (TvarT "b"))))), m cons),
 ("ifthen", (ForallT "a" (ForallT "b" (ArrowT (TupT [ArrowT (TvarT "a") BoolT, ArrowT (TvarT "a") (TvarT "b")]) (ArrowT (TvarT "a") (TvarT "b")))), m cond)),
 -- ("ifelse", (ForallT "a"
 --             (ForallT "b"
 --              (ForallT "c"
 --               (ArrowT (TupT [ArrowT (TvarT "a") BoolT,
 --                              ArrowT (TvarT "a") (TvarT "b"),
 --                              ArrowT (TvarT "a") (TvarT "c")])
 --                (ArrowT (TvarT "a") DynT))))), m cond),
 -- ("ifelse", ArrowT (TupT [ArrowT DynT BoolT, ArrowT DynT DynT, ArrowT DynT DynT]) (ArrowT DynT DynT), m cond),
--  ("cond", ArrowT (SeqT (ArrowT DynT DynT)) (ArrowT DynT DynT), m cond),
--  ("apply", ArrowT (TupT [ArrowT DynT DynT, DynT]) DynT, FnExpr apply),
--  ("lift", ArrowT (ArrowT DynT DynT) (ArrowT DynT DynT), m lift),
--  ("raise", m raise),
-- predicate combining forms
--  ("pcons", ArrowT (SeqT predT) predT, m pcons),
--  ("seqof", ArrowT predT predT, m seqof),
--  ("lenis", ArrowT IntT listPredT, m lenis),
--  ("&&", ArrowT (SeqT predT) predT, m (&&)),
--  ("||", ArrowT (SeqT predT) predT, m (||)),
--  ("¬", ArrowT predT predT, m (¬)),
--  ("=>", ArrowT (TupT [predT, predT]) listPredT, m rarrow),
--  ("<=", ArrowT (TupT [predT, predT]) listPredT, m (<=)),
--  ("map", ArrowT (ArrowT DynT DynT) (ArrowT (SeqT DynT) (SeqT DynT)), m alpha),
--  ("al", ArrowT (SeqT DynT) (SeqT DynT), m al),
--  ("ar", ArrowT (SeqT DynT) (SeqT DynT), m ar),
--  ("cat", ArrowT (SeqT (SeqT DynT)) (SeqT DynT), m cat),
--  ("distl", ArrowT (SeqT DynT) (SeqT DynT), m distl),
--  ("distr", ArrowT (SeqT DynT) (SeqT DynT), m distr),
--  ("len", ArrowT (SeqT DynT) IntT, m len),
--  ("reverse", ArrowT (SeqT DynT) (SeqT DynT), m reverse),
--  ("sel", ArrowT (SeqT DynT) DynT, m sel),
--  ("s", ArrowT IntT (ArrowT (SeqT DynT) DynT), m s),
 -- ("hd", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hd),
 -- ("tl", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tl),
 -- ("hdr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hdr),
 -- ("tlr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tlr),
--  -- io
--  ("out", ArrowT (SeqT (SeqT CharT)) (SeqT (SeqT CharT)), m out),
--  -- misc
 ("id", (ForallT "a" (ArrowT (TvarT "a") (TvarT "a")), m id)),
 -- ("signal", ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (TvarT "b"))), m signal),

 ("K", (ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (ArrowT (TvarT "b") (TvarT "a")))), m k))]


srcfile :: SrcFile
srcfile = SrcFile "Core" [] Nothing $ Right $ Map.fromList syms