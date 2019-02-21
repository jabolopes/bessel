{-# LANGUAGE LambdaCase #-}
module Test.Typechecker.Typechecker where

import Control.Monad
import Control.Monad.State hiding (state)
import Data.Char (ord)

import Data.Expr (Expr(..))
import qualified Data.Expr as Expr
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source as Source (Source(..))
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Pretty.Data.Source as Pretty
import qualified Stage.Expander as Expander
import Stage.Renamer (RenamerState)
import qualified Stage.Renamer as Renamer
import Typechecker.Context (Context, ContextVar)
import qualified Typechecker.Context as Context
import Typechecker.Type (Type(..))
import qualified Typechecker.Type as Type
import qualified Typechecker.Typechecker as Typechecker
import qualified Typechecker.TypeName as TypeName
import qualified Test.Diff as Diff

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

initialRenamerState :: Monad m => m RenamerState
initialRenamerState =
  case snd <$> runStateT getRenamerState Renamer.initialRenamerState of
    Left err -> fail $ show err
    Right state -> return state
  where
    getRenamerState =
      do Renamer.addFnSymbolM "true#" "true#"
         Renamer.addFnSymbolM "true" "true#"
         Renamer.addFnSymbolM "false#" "false#"
         Renamer.addFnSymbolM "false" "false#"
         Renamer.addFnSymbolM "+" "+"
         Renamer.addFnSymbolM ">" ">"
         Renamer.addFnSymbolM "addIntReal" "addIntReal"
         Renamer.addFnSymbolM "isInt" "isInt#"
         Renamer.addFnSymbolM "isReal" "isReal"
         Renamer.addFnSymbolM "isChar" "isChar#"
         Renamer.addFnSymbolM "isList#" "isList#"
         Renamer.addFnSymbolM "null#" "null#"
         Renamer.addFnSymbolM "null" "null#"
         Renamer.addFnSymbolM "case" "case"
         Renamer.addFnSymbolM "cons" "cons"
         Renamer.addFnSymbolM "head#" "head#"
         Renamer.addFnSymbolM "tail#" "tail#"
         Renamer.addFnSymbolM "isHeadTail#" "isHeadTail#"
         Renamer.addFnSymbolM "eqInt" "eqInt"
         -- Tuple
         Renamer.addFnSymbolM "isTuple0" "isTuple0"
         Renamer.addFnSymbolM "isTuple2" "isTuple2"
         Renamer.addFnSymbolM "mkTuple0" "mkTuple0"
         Renamer.addFnSymbolM "mkTuple2" "mkTuple2"
         Renamer.addFnSymbolM "tuple2Ref0#" "tuple2Ref0#"
         Renamer.addFnSymbolM "tuple2Ref1#" "tuple2Ref1#"
         -- Type
         Renamer.addFnSymbolM "isType#" "isType#"
         -- Variant
         Renamer.addFnSymbolM "isVariant#" "isVariant#"
         Renamer.addFnSymbolM "mkVariant#" "mkVariant#"
         Renamer.addFnSymbolM "unVariant#" "unVariant#"

lookupTypedNames :: Context -> Expr -> [Name]
lookupTypedNames context = lookupNames
  where
    lookupTypedName :: Name -> [Name]
    lookupTypedName name =
      let name' = Typechecker.lookupName context name in
      case Name.nameType name' of
        Nothing -> []
        _ -> [name']

    lookupNames :: Expr -> [Name]
    lookupNames (AnnotationE expr _) =
      lookupNames expr
    lookupNames (AppE fn arg) =
      lookupNames fn ++ lookupNames arg
    lookupNames (CondE matches) =
      concat $ (forM matches $ \(match, body) ->
                   lookupNames match ++ lookupNames body)
    lookupNames (FnDecl _ name expr) =
      lookupTypedName name ++ lookupNames expr
    lookupNames IdE {} =
      []
    lookupNames (LambdaE name expr) =
      lookupTypedName name ++ lookupNames expr
    lookupNames (LetE defn body) =
      lookupNames defn ++ lookupNames body
    lookupNames LiteralE {} =
      []

typecheckTestFile :: Context -> String -> IO (Either PrettyString [Name])
typecheckTestFile initialContext filename = Right <$> typecheckFile
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

    renameExprs :: Monad m => RenamerState -> [Expr] -> m [Expr]
    renameExprs _ [] = return []
    renameExprs state (expr:exprs) =
      case runStateT (Renamer.renameM expr) state of
        Left err -> fail $ show err
        Right (exprs', state') -> (exprs' ++) <$> renameExprs state' exprs

    renameFile =
      do exprs <- expandFile
         state <- initialRenamerState
         renameExprs state exprs

    typecheckExprs _ [] = return []
    typecheckExprs context (expr:exprs) =
      do putStrLn $ PrettyString.toString $ Pretty.docExpr expr
         case Typechecker.typecheck context expr Nothing of
           Left err ->
             fail $ show (err :: PrettyString)
           Right (context', _) ->
             do let names = lookupTypedNames context' expr
                (names++) <$> typecheckExprs context' exprs

    typecheckFile =
      do exprs <- renameFile
         typecheckExprs initialContext exprs

testTypechecker :: Bool -> IO ()
testTypechecker generateTestExpectations =
  do -- TODO: The following test cases should be running the typechecker in check mode.
     expect "Test/TypeTestData1.typechecker" "Test/TypeTestData1.bsl"
     expect "Test/TypeTestData2.typechecker" "Test/TypeTestData2.bsl"
     expect "Test/TypeTestData3.typechecker" "Test/TypeTestData3.bsl"

     expect "Test/TestData1.typechecker" "Test/TestData1.bsl"
     expect "Test/TestData2.typechecker" "Test/TestData2.bsl"
     expect "Test/TestData3.typechecker" "Test/TestData3.bsl"
     -- Note that 'Test/TestData4.bsl' is not typecheckable because
     -- the main function accepts both Int and List.
     expect "Test/TestData5.typechecker" "Test/TestData5.bsl"
     expect "Test/TestData6.typechecker" "Test/TestData6.bsl"
     expect "Test/TestData7.typechecker" "Test/TestData7.bsl"
     expect "Test/Tuple.typechecker" "Test/Tuple.bsl"
     expect "Test/Unit.typechecker" "Test/Unit.bsl"
     expect "Test/Variant.typechecker" "Test/Variant.bsl"

  where
    initialBindings =
      [("true#", Type.boolT),
       ("true", Type.boolT),
       ("false#", Type.boolT),
       ("false", Type.boolT),
       ("+", Arrow Type.intT (Arrow Type.intT Type.intT)),
       ("addIntReal", Arrow Type.intT (Arrow Type.realT Type.realT)),
       (">", Arrow Type.intT (Arrow Type.intT Type.boolT)),
       ("isInt#", Arrow Type.intT Type.boolT),
       ("isReal", Arrow Type.realT Type.boolT),
       ("isChar#", Arrow Type.charT Type.boolT),
       ("isList#", forall 'a' (Arrow (Type.ListT (Arrow (typeVar 'a') Type.boolT)) (Arrow (Type.ListT (typeVar 'a')) Type.boolT))),
       ("isHeadTail#", forall 'a' (Arrow (Arrow (typeVar 'a') Type.boolT) (Arrow (Arrow (Type.ListT (typeVar 'a')) Type.boolT) (Arrow (Type.ListT (typeVar 'a')) Type.boolT)))),
       ("null#", forall 'a' (Type.ListT (typeVar 'a'))),
       ("head#", forall 'a' (Arrow (Type.ListT (typeVar 'a')) (typeVar 'a'))),
       ("tail#", forall 'a' (Arrow (Type.ListT (typeVar 'a')) (Type.ListT (typeVar 'a')))),
       ("eqInt", Arrow Type.intT (Arrow Type.intT Type.boolT)),
       ("cons", Arrow Type.intT (Arrow (Type.ListT Type.intT) (Type.ListT Type.intT))),
       ("case", forall 'a' (forall 'b' (Arrow (typeVar 'a') (Arrow (Arrow (typeVar 'a') (typeVar 'b')) (typeVar 'b'))))),
       -- Tuple
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
       ("tuple2Ref0#", forall 'a' (forall 'b' (Arrow (TupleT [typeVar 'a', typeVar 'b']) (typeVar 'a')))),
       ("tuple2Ref1#", forall 'a' (forall 'b' (Arrow (TupleT [typeVar 'a', typeVar 'b']) (typeVar 'b')))),
       -- Type
       ("isType#", forall 'a' (Arrow Type.stringT (Arrow (typeVar 'a') Type.boolT))),
       -- Variant
       ("isVariant#",
        forall 'a'
        (forall 'b'
         (Arrow Type.stringT
          (Arrow Type.intT
           (Arrow (Arrow (typeVar 'a') Type.boolT)
            (Arrow (typeVar 'b')
             Type.boolT)))))),
       ("mkVariant#", forall 'a' (forall 'b' (Arrow Type.stringT (Arrow Type.intT (Arrow (typeVar 'a') (typeVar 'b')))))),
       ("unVariant#", forall 'a' (forall 'b' (Arrow (typeVar 'a') (typeVar 'b'))))
      ]

    initialContext =
      map (\(name, typ) -> Context.assigned (Name.untyped name) typ) initialBindings

    expect expectedFilename filename =
      do result <- typecheckTestFile initialContext filename
         let actual = PrettyString.toString . PrettyString.vcat . map (PrettyString.text . show) <$> result
         Diff.expectFiles "Typechecker" filename generateTestExpectations expectedFilename actual
