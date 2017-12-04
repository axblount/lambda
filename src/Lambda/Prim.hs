module Lambda.Prim
  ( Expr (..)
  , evalLambda
  ) where

import Prelude

data SourceInfo = SourceInfo {
  sourceName :: String,
  lineNumber :: Int,
  columnNumber :: Int
}

data Expr = Index !Int
          | Abst Expr
          | Appl Expr Expr
          deriving (Eq)

instance Show Expr where
    show = showExpr (map return "abcdefghijklmnopqrstuvwxyz") 0

showExpr :: [String] -> Int -> Expr -> String
showExpr names idx (Index i) = gensym names (idx-i-1)
showExpr names idx (Abst e) = "\\" ++ gensym names idx ++ "." ++ showExpr names (idx+1) e
showExpr n p (Appl f@(Abst _) x@(Appl _ _)) = "(" ++ showExpr n p f ++ ") (" ++ showExpr n p x ++ ")"
showExpr n p (Appl f          x@(Appl _ _)) = showExpr n p f ++ " (" ++ showExpr n p x ++ ")"
showExpr n p (Appl f@(Abst _) x           ) = "(" ++ showExpr n p f ++ ") " ++ showExpr n p x
showExpr n p (Appl f          x           ) = showExpr n p f ++ " " ++ showExpr n p x

gensym :: [String] -> Int -> String
gensym names idx =
            let (c, i) = divMod idx (length names)
                in names !! i ++ duplicate "'" c
  where duplicate s n = concat $ replicate n s


shift :: Int -> Int -> Expr -> Expr
shift d c (Index i) = Index $ if i < c then i else i + d
shift d c (Abst e) = Abst $ shift d (c+1) e
shift d c (Appl f x) = Appl (shift d c f) (shift d c x)

subst :: Expr -> Int -> Expr -> Expr
subst s j (Index i) = if i == j then s else Index i
subst s j (Abst e) = Abst $ subst (shift 1 0 s) (j+1) e
subst s j (Appl f x) = Appl (subst s j f) (subst s j x)

evalLambda1 :: Expr -> Expr
evalLambda1 (Appl (Abst e) x) = shift (-1) 0 $ subst (shift 1 0 (evalLambda x)) 0 e
evalLambda1 (Appl f x) = Appl (evalLambda f) x
evalLambda1 x@_ = x

evalLambda :: Expr -> Expr
evalLambda e =
  let e' = evalLambda1 e in
    if e' == e
      then e
      else evalLambda e'

{-
PRIMITIVES

tru = Abst $ Abst $ Index 1
fls = Abst $ Abst $ Index 0
test = Abst $ Abst $ Abst $ Appl (Appl (Index 2) (Index 1)) (Index 0)
and = Abst $ Abst $ Appl (Appl (Index 1) (Index 0)) fls

pair = Abst $ Abst $ Abst $ Appl (Appl (Index 0) (Index 2)) (Index 1)
fst = Abst $ Appl (Index 0) tru
snd = Abst $ Appl (Index 0) fls

zero = fls
succ = Abst $ Abst $ Abst $ Appl (Index 1) $ Appl (Appl (Index 2) (Index 1)) (Index 0)
-}