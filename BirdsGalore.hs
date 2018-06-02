module BirdsGalore where

import Control.Applicative((<|>),empty)
import Control.Arrow((&&&))
import Control.Monad(forM_)

import qualified Data.Set as Set

------------------------------------------------------------------------
-- Expression type and pretty-printer

data Expr = Ap Expr Expr
          | W
          | K
          | Var Integer
            deriving (Eq, Ord)

instance Show Expr where
  show e = show' False e

show' needBracket (Ap e1 e2) =
  let e = show' False e1 ++ " " ++ show' True e2
  in if needBracket then"(" ++ e ++ ")" else e
show' _ W = "W"
show' _ K = "K"
show' _ (Var i) = "X" ++ show i

------------------------------------------------------------------------
-- Build and reduce expressions

exprOfSize :: Integer -> [Expr]
exprOfSize 0 = []
exprOfSize 1 = [W, K]
exprOfSize n = concatMap aux [1..n - 1] where
  aux i = Ap <$> exprOfSize i <*> exprOfSize (n - i)

step :: Expr -> Maybe Expr
-- Actual combinator cases
step (Ap (Ap W x) y) = Just $ Ap (Ap x y) y
step (Ap (Ap K x) y) = Just $ x
-- No match? Then try digging down the left.
step (Ap l r) = flip Ap r <$> step l
-- Still didn't work?
step _ = Nothing

-- Guarantee termination
boundSteps :: Integer -> Expr -> Maybe Expr
boundSteps 0 _ = empty
boundSteps i x = case step x of
  Just x' -> boundSteps (i - 1) x'
  Nothing -> return x

------------------------------------------------------------------------
-- Print the reductions

allExprs = concatMap exprOfSize [1..]

maxSteps = 10

-- Problem 13: Build a Mockinbird from W and K.

isAMockingbird e = reduced == target where
  reduced = boundSteps maxSteps $ Ap e (Var 1)
  target = Just $ Ap (Var 1) (Var 1)

main = do
  print $ head $ filter isAMockingbird allExprs
