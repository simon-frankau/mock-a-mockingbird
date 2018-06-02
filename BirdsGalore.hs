module BirdsGalore where

import Control.Arrow((&&&))
import Control.Monad(forM_)

import Data.Maybe(fromJust)

------------------------------------------------------------------------
-- Expression type and pretty-printer

data Expr = Ap Expr Expr
          | W
          | K
          | M
          | I
          | C
          | T
          | Var Integer
            deriving (Eq, Ord)

instance Show Expr where
  show e = show' False e

show' needBracket (Ap e1 e2) =
  let e = show' False e1 ++ " " ++ show' True e2
  in if needBracket then "(" ++ e ++ ")" else e
show' _ W = "W"
show' _ K = "K"
show' _ M = "M"
show' _ I = "I"
show' _ C = "C"
show' _ T = "T"
show' _ (Var i) = "X" ++ show i

------------------------------------------------------------------------
-- Build and reduce expressions

-- Build expression with limited leaves.
exprOfSize :: [Expr] -> Integer -> [Expr]
exprOfSize xs = aux where
  aux 0 = []
  aux 1 = xs
  aux n = concatMap doAp [1..n - 1] where
    doAp i = Ap <$> aux i <*> aux (n - i)

step :: Expr -> Maybe Expr
-- Actual combinator cases
step     (Ap (Ap W x) y)    = Just $ Ap (Ap x y) y
step     (Ap (Ap K x) y)    = Just $ x
step         (Ap M x)       = Just $ Ap x x
step         (Ap I x)       = Just $ x
step (Ap (Ap (Ap C x) y) z) = Just $ Ap (Ap x z) y
step     (Ap (Ap T x) y)    = Just $ Ap y x
-- No match? Then try digging down the left.
step (Ap l r) = flip Ap r <$> step l
-- Still didn't work?
step _ = Nothing

-- Guarantee termination
boundSteps :: Integer -> Expr -> Maybe Expr
boundSteps 0 _ = Nothing
boundSteps i x = case step x of
  Just x' -> boundSteps (i - 1) x'
  Nothing -> return x

------------------------------------------------------------------------
-- Print the reductions

allExprsOf xs = concatMap (exprOfSize xs) [1..]

maxSteps = 10

isOnlyVars (Ap x y) = isOnlyVars x && isOnlyVars y
isOnlyVars (Var _)  = True
isOnlyVars _        = False

applyNVars :: Expr -> Integer -> Expr
applyNVars e = aux where
  aux 0 = e
  aux i = Ap (aux $ i - 1) (Var i)

-- Find the number of vars required to fully reduce an expression
findNVars :: Expr -> Integer
findNVars e = fst $ head $
              filter (isOnlyVars . snd) $
              map (id &&& (fromJust . boundSteps maxSteps . applyNVars e)) $
              [0..]

-- Find a way to build a combinator using a different set of combinators
findExpr :: Expr -> [Expr] -> Expr
findExpr target avail = head $ filter isTarget $ allExprsOf avail where
  numVars = findNVars target
  applyReduce = boundSteps maxSteps . flip applyNVars numVars
  targetWithVars = applyReduce target
  isTarget candidate = applyReduce candidate == targetWithVars

-- Find an pretty-print a solution
solve :: Integer -> Expr -> [Expr] -> IO ()
solve problemNum target avail = do
  let solution = findExpr target avail
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show target ++ " = " ++ show solution

main = do
  solve 13 M [W, K]
  solve 14 M [W, I]
  solve 15 I [W, K]
  solve 16 I [C, K]
  solve 17 T [C, I]
