import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))


data Stx = IntStx Int
         | SeqStx [Stx]
         | CatchStx Stx Stx
         | ErrorStx String
         deriving (Eq, Show)


data Expr = IntExpr Int
          | SeqExpr [Expr]
          deriving (Eq, Show)


type EvalErrorM = Either String
type EvalM a = StateT Expr EvalErrorM a


evalM :: Stx -> EvalM Expr
evalM (IntStx i) = return $ IntExpr i
evalM (SeqStx stxs) = SeqExpr <$> mapM evalM stxs

evalM (CatchStx stx1 stx2) =
    evalM stx1 `catchError` const (evalM stx2)

evalM (ErrorStx str) = throwError str


eval :: Stx -> EvalErrorM Expr
eval stx =
    evalStateT (evalM stx) (IntExpr 0)


e1 = eval $ IntStx 0

e2 = eval $ SeqStx [IntStx 0, IntStx 1]

e3 = eval $ SeqStx [IntStx 0, ErrorStx "error", IntStx 1]

e4 = eval $ CatchStx (SeqStx [IntStx 0, ErrorStx "error", IntStx 1]) (IntStx 10)