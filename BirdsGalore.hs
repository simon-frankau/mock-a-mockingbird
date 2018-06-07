module BirdsGalore where

import Control.Arrow((&&&))
import Control.Monad(forM_)

import Data.Maybe(fromJust)

------------------------------------------------------------------------
-- Expression type and pretty-printer

data Combinator = W | K | M | I | C | T | B | R | F | E | V | CStar | Q
                  deriving (Show, Eq, Ord)

data Expr = Ap Expr Expr
          | Co Combinator
          | Var Integer
            deriving (Eq, Ord)

instance Show Expr where
  show e = show' False e

show' needBracket (Ap e1 e2) =
  let e = show' False e1 ++ " " ++ show' True e2
  in if needBracket then "(" ++ e ++ ")" else e
show' _ (Co  c) = show c
show' _ (Var i) = "X" ++ show i

------------------------------------------------------------------------
-- Build and reduce expressions

-- Build expression with limited leaves.
exprOfSize :: [Combinator] -> Integer -> [Expr]
exprOfSize xs = aux where
  aux 0 = []
  aux 1 = map Co xs
  aux n = concatMap doAp [1..n - 1] where
    doAp i = Ap <$> aux i <*> aux (n - i)

step :: Expr -> Maybe Expr
-- Actual combinator cases
step             (Ap (Ap (Co W) x) y)          = Just $ x # y # y
step             (Ap (Ap (Co K) x) y)          = Just $ x
step                 (Ap (Co M) x)             = Just $ x # x
step                 (Ap (Co I) x)             = Just $ x
step         (Ap (Ap (Ap (Co C) x) y) z)       = Just $ x # z # y
step             (Ap (Ap (Co T) x) y)          = Just $ y # x
step         (Ap (Ap (Ap (Co B) x) y) z)       = Just $ x # (y # z)
step         (Ap (Ap (Ap (Co R) x) y) z)       = Just $ y # z # x
step         (Ap (Ap (Ap (Co F) x) y) z)       = Just $ z # y # x
step (Ap (Ap (Ap (Ap (Ap (Co E) x) y) z) w) v) = Just $ x # y # (z # w # v)
step         (Ap (Ap (Ap (Co V) x) y) z)       = Just $ z # x # y
step         (Ap (Ap (Ap (Co Q) x) y) z)       = Just $ y # (x # z)
step (Ap (Ap (Ap (Ap (Co CStar) x) y) z) w)    = Just $ x # y # w # z
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

a # b = Ap a b

------------------------------------------------------------------------
-- Print the reductions

allExprsOf xs = concatMap (exprOfSize xs) [1..]

maxSteps = 10

isOnlyVars (Ap x y) = isOnlyVars x && isOnlyVars y
isOnlyVars (Var _)  = True
isOnlyVars (Co  _)  = False

applyNVars :: Expr -> Integer -> Expr
applyNVars e = aux where
  aux 0 = e
  aux i = Ap (aux $ i - 1) (Var i)

maxVar :: Expr -> Integer
maxVar (Ap x y) = maxVar x `max` maxVar y
maxVar (Var i) = i
maxVar _ = 0

-- Find the number of vars required to fully reduce an expression
findNVars :: Expr -> Integer
findNVars e = fst $ head $
              filter (isOnlyVars . snd) $
              map (id &&& (fromJust . boundSteps maxSteps . applyNVars e)) $
              [0..]

-- Find a way to build a combinator using a different set of combinators
findExpr :: Combinator -> [Combinator] -> Expr
findExpr target avail = head $ filter isTarget $ allExprsOf avail where
  targetExpr = Co target
  numVars = findNVars targetExpr
  applyReduce = boundSteps maxSteps . flip applyNVars numVars
  targetWithVars = applyReduce targetExpr
  isTarget candidate = applyReduce candidate == targetWithVars

-- Find an expression that permutes the supplied parameters as needed
findExpr' :: Expr -> [Combinator] -> Expr
findExpr' target avail = head $ filter isTarget $ allExprsOf avail where
  numVars = maxVar target
  applyReduce = boundSteps maxSteps . flip applyNVars numVars
  isTarget candidate = applyReduce candidate == Just target

-- Find an pretty-print a solution
solve :: Integer -> Combinator -> [Combinator] -> IO ()
solve problemNum target avail = do
  let solution = findExpr target avail
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show target ++ " = " ++ show solution

solve' :: Integer -> Expr -> [Combinator] -> IO ()
solve' problemNum target avail = do
  let solution = findExpr' target avail
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show target ++ " = " ++ show solution

main = do
  solve 13 M [W, K]
  solve 14 M [W, I]
  solve 15 I [W, K]
  solve 16 I [C, K]
  solve 17 T [C, I]
  solve 20 R [B, T]
  solve 21 C [R]
  solve 21 C [B, T]
  solve 23 R [C]
  solve 24 F [B, R, C]
  solve 25 F [T, E]
  solve 26 F [B, T]
  solve 27 V [C, F]
  -- Too slow - search space is too big.
  -- solve 27 V [B, T]
  -- Instead, we know V = C F
  let c = findExpr C [B, T]
      f = findExpr F [B, T]
      v = Ap c f
  putStrLn $ "Problem 27: V = " ++
             show (fromJust $ boundSteps maxSteps v)
  solve 28 V [F, R]
  solve 29 F [C, V]
  solve 30 I [R, K]
  -- For the ones where we're not reusing the combinators, don't
  -- bother adding them as actual combinators.
  let (x, y, z, w, v) = (Var 1, Var 2, Var 3, Var 4, Var 5)
  solve' 31 (x # y # w # z) [B, C]
  solve' 32 (x # z # w # y) [B, C]
  solve' 33 (x # w # z # y) [B, C]
  solve' 34 (x # w # y # z) [B, C] -- Typo in the book
  solve' 35 (x # y # z # v # w) [B, C]
  solve' 35 (x # y # w # v # z) [B, C]
  solve' 35 (x # y # v # w # z) [B, C]
  solve' 35 (x # y # v # z # w) [B, C]
  solve 36 V [CStar, T]
  solve' 37 (y # (x # z)) [B, C]
  -- TODO! Too slow...
  -- solve' 38 (x # (z # y)) [B, T]
  solve' 39 (y # (z # x)) [B, T]
  solve' 41 (z # (x # y)) [B, T]
  solve' 42 (z # (y # x)) [B, T]
  solve 45 B [Q, T]
  solve 46 C [Q, T]
  -- TODO: Too slow:
  -- solve' 47 (x # w # (y # z)) [B, T]
