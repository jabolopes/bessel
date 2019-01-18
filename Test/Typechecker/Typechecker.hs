{-# LANGUAGE LambdaCase #-}
module Test.Typechecker.Typechecker where

import Control.Monad
import Data.Char (ord)

import Data.Expr
import qualified Data.Expr as Expr
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import Data.Source as Source (Source(..))
import qualified Parser
import qualified Stage.Expander as Expander
import Typechecker.Context (Context, ContextVar)
import qualified Typechecker.Context as Context
import Typechecker.Type (Type(..))
import qualified Typechecker.Type as Type
import Typechecker.Typechecker (typecheck)
import qualified Typechecker.Typechecker as Typechecker
import qualified Typechecker.TypeName as TypeName

import qualified Data.PrettyString as PrettyString
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Data.Expr as Pretty
import qualified Pretty.Stage.Typechecker as Pretty

typeName :: Char -> Int
typeName c = ord c - ord 'a'

assigned :: Char -> Type -> (ContextVar, Maybe Type)
assigned c typ = Context.assigned (Name.untyped [c]) typ

lambda :: Expr -> Expr -> Expr
lambda (IdE arg) = LambdaE arg
lambda _ = error "lambda: expected identifier"

var :: Char -> Expr
var = Expr.idE . (:[])

existVar :: Char -> Type
existVar = ExistVar . TypeName.typeName

typeVar :: Char -> Type
typeVar = TypeVar . TypeName.typeName

forall :: Char -> Type -> Type
forall name = Forall (TypeName.typeName name)

typecheckTestFile :: Context -> String -> IO [Type]
typecheckTestFile initialContext filename = typecheckFile
  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile (Name.untyped filename) str of
           Left err -> fail err
           Right src -> do
             putStrLn $ PrettyString.toString $ Pretty.docSource src
             return src

    expandFile =
      do ModuleS _ _ srcs <- parseFile
         case concat `fmap` mapM Expander.expand srcs of
           Left err -> fail $ show err
           Right exprs ->
             do forM_ exprs $ putStrLn . PrettyString.toString . Pretty.docExpr
                return exprs

    typecheckExprs _ [] = return []
    typecheckExprs context (expr:exprs) =
      do putStrLn $ PrettyString.toString $ Pretty.docExpr expr
         case Typechecker.typecheck context expr Nothing of
           Left err -> fail $ show (err :: PrettyString)
           Right (context', typ) -> (typ:) <$> typecheckExprs context' exprs

    typecheckFile =
      do exprs <- expandFile
         typecheckExprs initialContext exprs

expectSnippet :: Context -> Type -> Expr -> Maybe Type -> IO ()
expectSnippet context expected snippet annotation =
  typecheck context snippet annotation >>= \case
    (_, actual) | expected == actual -> return ()
                | otherwise -> fail (PrettyString.toString $ Pretty.typeMismatch "(snippet)" (show actual) (show expected))

expect :: Context -> [Type] -> String -> IO ()
expect context expected filename =
  typecheckTestFile context filename >>= \case
    actual | expected == actual -> return ()
           | otherwise -> fail (PrettyString.toString $ Pretty.typeMismatch filename (show actual) (show expected))

testTypechecker :: IO ()
testTypechecker =
  do expect initialContext [Arrow Type.intT (Arrow Type.intT Type.intT)] "Test/TypeTestData1.bsl"
     expect initialContext [Type.intT `Arrow` Type.intT] "Test/TypeTestData2.bsl"
     expect initialContext [Arrow Type.intT (Arrow Type.intT Type.boolT)] "Test/TypeTestData3.bsl"

     expect initialContext [Arrow Type.intT (Arrow Type.intT Type.intT)] "Test/TestData1.bsl"
     expect initialContext [Type.intT `Arrow` Type.intT] "Test/TestData2.bsl"
     expect initialContext [Arrow Type.intT (Arrow Type.intT Type.boolT)] "Test/TestData3.bsl"
     expect initialContext [Arrow Type.intT (Arrow Type.intT Type.boolT)] "Test/TestData4.bsl"
     -- expect initialContext [Arrow Type.intT (Arrow Type.intT Type.boolT)] "Test/TestData5.bsl"
     expect initialContext [Type.intT `Arrow` Type.intT] "Test/TestData6.bsl"
     expect initialContext [ListT Type.intT, Type.intT, Type.intT] "Test/TestData7.bsl"
     -- expect initialContext [Arrow (PrimitiveT "Fruit") Type.intT] "Test/TestData8.bsl"
     -- Tuple
     expect initialContext [Arrow (TupleT [Type.intT, Type.realT]) Type.realT] "Test/Tuple.bsl"
     expect initialContext [Type.unitT `Arrow` Type.unitT, Type.unitT] "Test/Unit.bsl"

  where
    initialBindings =
      [("true#", Type.boolT),
       ("true", Type.boolT),
       ("false#", Type.boolT),
       ("false", Type.boolT),
       ("+", Arrow Type.intT (Arrow Type.intT Type.intT)),
       ("addIntReal", Arrow Type.intT (Arrow Type.realT Type.realT)),
       (">", Arrow Type.intT (Arrow Type.intT Type.boolT)),
       ("isInt", Arrow Type.intT Type.boolT),
       ("isReal", Arrow Type.realT Type.boolT),
       ("isList", forall 'a' (Arrow (Type.ListT (Arrow (typeVar 'a') Type.boolT)) (Arrow (Type.ListT (typeVar 'a')) Type.boolT))),
       ("isHeadTail", forall 'a' (Arrow (Arrow (typeVar 'a') Type.boolT) (Arrow (Arrow (Type.ListT (typeVar 'a')) Type.boolT) (Arrow (Type.ListT (typeVar 'a')) Type.boolT)))),
       ("null", forall 'a' (Type.ListT (typeVar 'a'))),
       ("hd", forall 'a' (Arrow (Type.ListT (typeVar 'a')) (typeVar 'a'))),
       ("tl", forall 'a' (Arrow (Type.ListT (typeVar 'a')) (Type.ListT (typeVar 'a')))),
       ("eqInt", Arrow Type.intT (Arrow Type.intT Type.boolT)),
       ("cons", Arrow Type.intT (Arrow (Type.ListT Type.intT) (Type.ListT Type.intT))),
       ("case", forall 'a' (forall 'b' (Arrow (typeVar 'a') (Arrow (Arrow (typeVar 'a') (typeVar 'b')) (typeVar 'b'))))),
       ("isApple", Arrow (Type.PrimitiveT "Fruit") Type.boolT),
       ("unCons#", forall 'a' (forall 'b' (Arrow (typeVar 'a') (typeVar 'b')))),
       -- Tuples
       ("isTuple0", Type.unitT `Arrow` Type.boolT),
       ("isTuple2",
        forall 'a'
         (forall 'b'
          (Arrow
            (TupleT [typeVar 'a' `Arrow` Type.boolT, typeVar 'b' `Arrow` Type.boolT])
            (Arrow
              (TupleT [typeVar 'a', typeVar 'b'])
              Type.boolT)))),
       ("mkTuple0", Type.unitT),
       ("mkTuple2",
        forall 'a'
         (forall 'b'
          (Arrow (typeVar 'a')
           (Arrow (typeVar 'b')
            (TupleT [typeVar 'a', typeVar 'b']))))),
       ("tuple2Ref0", forall 'a' (forall 'b' (Arrow (TupleT [typeVar 'a', typeVar 'b']) (typeVar 'a')))),
       ("tuple2Ref1", forall 'a' (forall 'b' (Arrow (TupleT [typeVar 'a', typeVar 'b']) (typeVar 'b'))))
      ]

    initialContext =
      map (\(name, typ) -> Context.assigned (Name.untyped name) typ) initialBindings
