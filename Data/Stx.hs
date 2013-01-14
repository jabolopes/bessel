{-# LANGUAGE ParallelListComp #-}
module Data.Stx where


data DefnKw
    = Def
    | NrDef
      deriving (Eq, Ord, Show)


data Namespace a
    = Namespace [String] [Stx a]
      deriving (Eq, Ord, Show)


data Stx a
    = CharStx Char
    | IntStx Int
    | DoubleStx Double
    | SeqStx [Stx a]

    | IdStx a

    | AppStx (Stx a) (Stx a)
    | DefnStx DefnKw String (Stx a)
    | LambdaStx String (Stx a)
    | ModuleStx String (Namespace a)
    | TypeStx String [Stx a]
    | TypeMkStx String
    | TypeUnStx
    | TypeIsStx String
    | WhereStx (Stx a) [Stx a]
      deriving (Eq, Ord, Show)


isCharStx :: Stx a -> Bool
isCharStx (CharStx _) = True
isCharStx _ = False


isModuleStx :: Stx a -> Bool
isModuleStx (ModuleStx _ _) = True
isModuleStx _ = False


isValueStx :: Stx a -> Bool
isValueStx (CharStx _) = True
isValueStx (IntStx _) = True
isValueStx (DoubleStx _) = True
isValueStx (SeqStx _) = True
isValueStx (IdStx _) = True
isValueStx (LambdaStx _ _) = True
isValueStx _ = False


appStx :: a -> Stx a -> Stx a
appStx str stx = AppStx (IdStx str) stx


applyStx :: a -> [Stx a] -> Stx a
applyStx str = appStx str . SeqStx


signalStx :: String -> String -> Stx String
signalStx id str =
    appStx "o" (SeqStx [IdStx "signal",
                        appStx "cons" (SeqStx [appStx "K" $ stringStx id,
                                               appStx "K" $ stringStx str,
                                               IdStx "id"])])

stringStx :: String -> Stx a
stringStx str = SeqStx $ map CharStx str