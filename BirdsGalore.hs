module BirdsGalore where

import Control.Applicative((<|>))
import Control.Arrow((&&&))
import Control.Monad(forM_)

import qualified Data.Set as Set
import Data.Maybe(fromJust)

------------------------------------------------------------------------
-- Expression type and pretty-printer

data Combinator = W | K | M | I | C | T | B | R | F | E | V | CStar |
                  Q | L | H | S | G | G1 | I2 | Phi | U | O | J | J1
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

-- Maximum number of times a combinator can be applied before it can be
-- fully reduced. "Nothing" if that doesn't apply.
maxApp :: Combinator -> Maybe Integer
maxApp W  = Nothing -- Repeated use of variable means it can expand expressions.
maxApp K  = Just 2
maxApp M  = Nothing -- Ditto
maxApp I  = Just 1
maxApp C  = Just 3
maxApp T  = Just 2
maxApp B  = Just 3
maxApp R  = Just 3
maxApp F  = Just 3
maxApp E  = Just 5
maxApp V  = Just 3
maxApp CStar = Just 4
maxApp Q  = Just 3
maxApp L  = Nothing
maxApp H  = Nothing
maxApp S  = Nothing
maxApp G  = Just 4
maxApp G1 = Just 5
maxApp I2 = Nothing
maxApp Phi = Nothing
maxApp U = Nothing
maxApp O = Nothing
maxApp J = Nothing
maxApp J1 = Nothing

i `lessThanMaxApp` x =
  case maxApp x of
    Just j  -> i < j
    Nothing -> True

-- Build expression with limited leaves.
exprOfSize :: [Combinator] -> Integer -> [Expr]
exprOfSize xs = aux 0 where
  -- First arg is number of applies we're inside, second is number of
  -- leaves (combinators) we want.
  aux _ 0 = []
  aux i 1 = map Co $ filter (i `lessThanMaxApp`) xs
  aux i n = concatMap doAp [1..n - 1] where
    doAp j = Ap <$> aux (i + 1)j <*> aux 0 (n - j)

step :: Expr -> Maybe Expr
-- Actual combinator cases
step             (Ap (Ap (Co W)  x) y)          = Just $ x # y # y
step             (Ap (Ap (Co K)  x) y)          = Just $ x
step                 (Ap (Co M)  x)             = Just $ x # x
step                 (Ap (Co I)  x)             = Just $ x
step                 (Ap (Co I2) x)             = Just $ x # Co I # Co I
step         (Ap (Ap (Ap (Co C)  x) y) z)       = Just $ x # z # y
step             (Ap (Ap (Co T)  x) y)          = Just $ y # x
step         (Ap (Ap (Ap (Co B)  x) y) z)       = Just $ x # (y # z)
step         (Ap (Ap (Ap (Co R)  x) y) z)       = Just $ y # z # x
step         (Ap (Ap (Ap (Co F)  x) y) z)       = Just $ z # y # x
step (Ap (Ap (Ap (Ap (Ap (Co E)  x) y) z) w) v) = Just $ x # y # (z # w # v)
step         (Ap (Ap (Ap (Co V)  x) y) z)       = Just $ z # x # y
step         (Ap (Ap (Ap (Co Q)  x) y) z)       = Just $ y # (x # z)
step             (Ap (Ap (Co L)  x) y)          = Just $ x # (y # y)
step         (Ap (Ap (Ap (Co H)  x) y) z)       = Just $ x # y # z # y
step         (Ap (Ap (Ap (Co S)  x) y) z)       = Just $ x # z # (y # z)
step     (Ap (Ap (Ap (Ap (Co G)  x) y) z) w)    = Just $ x # w # (y # z)
step (Ap (Ap (Ap (Ap (Co CStar)  x) y) z) w)    = Just $ x # y # w # z
step (Ap (Ap (Ap (Ap (Ap (Co G1) x) y) z) w) v) = Just $ x # y # v # (z # w)
step     (Ap (Ap (Ap (Ap (Co Phi) x) y) z) w)   = Just $ x # (y # w) # (z # w)
step             (Ap (Ap (Co U) x) y)           = Just $ y # (x # x # y)
step             (Ap (Ap (Co O) x) y)           = Just $ y # (x # y)
step     (Ap (Ap (Ap (Ap (Co J) x) y) z) w)     = Just $ x # y # (x # w # z)
step     (Ap (Ap (Ap (Ap (Co J1) x) y) z) w)    = Just $ y # x # (w # x # z)
-- No match? Then try digging down further, left biasing.
step (Ap l r) = (flip Ap r <$> step l) <|> (Ap l <$> step r)
-- Still didn't work?
step _ = Nothing

-- Guarantee termination
boundSteps :: Integer -> Expr -> Maybe Expr
boundSteps 0 _ = Nothing
boundSteps i x = case step x of
  Just x' -> boundSteps (i - 1) x'
  Nothing -> return x

class Exprable a where
  exprise :: a -> Expr

instance Exprable Expr where
  exprise = id

instance Exprable Combinator where
  exprise = Co

a # b = Ap (exprise a) (exprise b)

------------------------------------------------------------------------
-- Print the reductions

allExprsOf xs = concatMap (exprOfSize xs) [1..]

maxSteps = 10

-- Adding more variables will do nothing when the top-level
-- left-hand-most leaf node is a variable.
enoughVars (Ap x y) = enoughVars x
enoughVars (Var _)  = True
enoughVars (Co  _)  = False

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
              filter (enoughVars . snd) $
              map (id &&& (fromJust . boundSteps maxSteps . applyNVars e)) $
              [0..]

applyReduce :: Expr -> Integer -> Maybe Expr
applyReduce e i = boundSteps maxSteps $ applyNVars e i

class GetTarget a where
  -- Returns the reduced expression and the number of variables needed
  getTarget :: a -> (Maybe Expr, Integer)

instance GetTarget Combinator where
  getTarget target = (targetWithVars, numVars) where
    targetExpr = Co target
    numVars = findNVars targetExpr
    targetWithVars = applyReduce targetExpr numVars

instance GetTarget Expr where
  getTarget target = (Just target, maxVar target) where

-- Find a way to build a combinator using a different set of combinators
findExpr :: GetTarget a => a -> [Combinator] -> Expr
findExpr target avail = head $ filter isTarget $ allExprsOf avail where
  (targetWithVars, nVars) = getTarget target
  isTarget candidate = applyReduce candidate nVars == targetWithVars

------------------------------------------------------------------------
-- Fixed point stuff

-- Check if an expression forms a fixed-point operator. Written in
-- quite an imperative style as I'm lazy and bad.
isFixedPoint :: Expr -> Bool
isFixedPoint expr = aux maxSteps (expr # x) Set.empty where
  aux stepsLeft expr seen = case step expr of
    -- Is it of form (x E), where E is some expression we saw before?
    Just e@(Ap l r) | l == x && r `Set.member` seen ->
      True
    -- Not seen before, but we have steps left? Then iterate.
    Just e | stepsLeft > 0 ->
      aux (stepsLeft - 1) e (e `Set.insert` seen)
    -- Otherwise, it's no fixed-point operator
    _ ->
       False

findFixedPoint :: [Combinator] -> Expr
findFixedPoint = head . filter isFixedPoint . allExprsOf

------------------------------------------------------------------------
-- SK converters

hasVar :: Integer -> Expr -> Bool
hasVar i = aux where
  aux (Ap l r) = aux l || aux r
  aux (Co _)   = False
  aux (Var j)  = i == j

elimVar :: Integer -> Expr -> Expr
elimVar i = aux where
  aux :: Expr -> Expr
  aux (Var j)        |                     i == j = Co I
  aux e              | not (hasVar i e)           = K # e
  aux (Ap l (Var j)) | not (hasVar i l) && i == j = l
  aux (Ap l r)                                    = S # aux l # aux r

-- Convert a set of variables into an SK combinator
skify :: Expr -> Expr
skify e = foldl (flip elimVar) e $ reverse [1 .. maxVar e]

------------------------------------------------------------------------
-- BCSI converters

elimVarBCSI :: Integer -> Expr -> Expr
elimVarBCSI i = aux where
  aux :: Expr -> Expr
  aux (Var j)
    | i == j = Co I
  aux (Ap l (Var j))
    | not (hasVar i l) && i == j = l
  aux (Ap l r)
    | hasVar i l && hasVar i r = S # aux l # aux r
  aux (Ap l r)
    | hasVar i l = C # aux l # r
  aux (Ap l r)
    | hasVar i r = B # l # aux r
  aux e = error $ "Var " ++ show i ++ " does not occur in " ++ show e

-- Convert a set of variables into a BCSI combinator
bcsiify :: Expr -> Expr
bcsiify e = foldl (flip elimVarBCSI) e $ reverse [1 .. maxVar e]

------------------------------------------------------------------------
-- Solvers

-- Find and pretty-print a solution
solve :: (GetTarget a, Show a) =>
         Integer -> a -> [Combinator] -> IO ()
solve problemNum target avail = do
  let solution = findExpr target avail
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show target ++ " = " ++ show solution

-- Perform a substitution over an expression
subst :: [(Combinator, Expr)] -> Expr -> Expr
subst mapping = aux where
  aux (Ap e1 e2) = Ap (aux e1) (aux e2)
  aux (Co c) = fromJust $ lookup c mapping
  aux v@(Var _) = v

-- Find and pretty-print a solution, using an intermediate set of
-- combinators.
solve2 :: (GetTarget a, Show a) =>
          Integer -> a -> [Combinator] -> [Combinator] -> IO ()
solve2 problemNum target avail1 avail2 = do
  let sol1 = findExpr target avail1
      -- Build a dictionary of how to express combinators in avail1 in
      -- terms of combinators in avail2.
      elts = map (id &&& flip findExpr avail2) avail1
      sol2 = fromJust $ boundSteps maxSteps $ subst elts sol1
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show target ++ " = " ++ show sol2

-- Find and pretty-print a fixed-point operator, using the given set
-- of combinators.
solveFP :: Integer -> [Combinator] -> IO ()
solveFP problemNum avail = do
  let solution = findFixedPoint avail
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++ show solution

-- Check the first expression reduces to the second
checkReduction :: Integer -> Expr -> Expr -> IO ()
checkReduction problemNum source target = do
  let reducedSource = case boundSteps maxSteps source of
        Just res -> res
        Nothing  -> source
      status = if reducedSource == target
               then "OK!"
               else "FAILED"
  putStrLn $ "Problem " ++ show problemNum ++ ": " ++
             show source ++ " -> " ++ show reducedSource ++ " vs " ++
             show target ++ " - " ++ status

(x, y, z, w, v) = (Var 1, Var 2, Var 3, Var 4, Var 5)

main = do
  putStrLn "Chapter 11"
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
  -- Instead, we go via V = C F
  solve2 27 V [C, F] [B, T]
  solve 28 V [F, R]
  solve 29 F [C, V]
  solve 30 I [R, K]
  -- For the ones where we're not reusing the combinators, don't
  -- bother adding them as actual combinators.
  solve 31 (x # y # w # z) [B, C]
  solve 32 (x # z # w # y) [B, C]
  solve 33 (x # w # z # y) [B, C]
  solve 34 (x # w # y # z) [B, C] -- Typo in the book
  solve 35 (x # y # z # v # w) [B, C]
  solve 35 (x # y # w # v # z) [B, C]
  solve 35 (x # y # v # w # z) [B, C]
  solve 35 (x # y # v # z # w) [B, C]
  solve 36 V [CStar, T]
  solve 37 (y # (x # z)) [B, C]
  -- Too slow...
  -- solve 38 (x # (z # y)) [B, T]
  solve2 38 (x # (z # y)) [B, C, T] [B, T]
  solve 39 (y # (z # x)) [B, T]
  solve 41 (z # (x # y)) [B, T]
  solve 42 (z # (y # x)) [B, T]
  solve 45 B [Q, T]
  solve 46 C [Q, T]
  -- Too slow...
  -- solve 47 G [B, T]
  solve2 47 G [B, C, T] [B, T]

  putStrLn "Chapter 12"
  solve 1 (x # y # (x # y)) [B, M]
  solve 2 L [B, C, M]
  solve 3 L [B, W]
  solve 4 L [M, Q]
  solve 5 (y # x # x) [B, R, M]
  solve 6 W [B, R, C, M]
  solve2 7 W [B, R, C, M] [B, T, M]
  solve 8 M [B, T, W]
  solve 9 (x # y # z # z) [B, T, C, M]
  solve 9 (x # y # z # w # w) [B, T, C, M]
  solve 10 H [B, C, W]
  solve2 10 H [B, C, W] [B, T, C, M]
  solve 11 W [C, H]
  solve 11 W [R, H]
  solve 12 S [B, W, G]
  solve 12 S [B, W, G]
  solve 12 S [B, C, W]
  solve 13 H [S, C]
  solve 13 H [S, R]
  solve 14 W [S, C]
  solve 14 W [S, R]
  solve 15 W [T, S]
  solve 16 M [T, S]

  putStrLn "Chapter 12 exercises"
  solve 1 G1 [B, C, T]
  let g2 = x # w # (x # w) # (y # z)
  solve 1 g2 [B, G1, M]
  solve 1 I2 [B, T, I]
  checkReduction 1 (I2 # (F # x)) x
  let g2Expr = findExpr g2 [B, G1, M]
  checkReduction 1 (g2Expr # F # (Q # I2) # x # y) (x # y # y)
  checkReduction 2 (B # (B # (B # W) # C) # (B # B) # x # y # z)
                   (x # z # (y # z))
  solve 3 (x # (y # w) # (z # w)) [S, B]
  solve 4 (x # (y # z) # (y # w)) [B, C, H, W]
  -- NB: Typo in book missed the extra 'z'.
  solve 5 (y # (z # w) # (x # y # z # w # v)) [Phi, B]
  solve 5 (x # (y # z) # (y # w)) [Phi, B, K]

  putStrLn "Chapter 13 exercises"
  solveFP  1   [M, B, R]
  solveFP  2   [B, C, M]
  solveFP  3   [M, B, L]
  solveFP  4   [M, B, W]
  solveFP  5   [B, C, W]
  solveFP  6   [Q, L, W]
  solveFP  8   [Q, M]
  solveFP  9   [S, L]
  solveFP 10   [B, W, S]
  -- "B, M, T or any dervied birds"
  solve   11 U [B, C, R, V, F, M, T, L, W, S]
  solveFP 12   [U]
  solve   13 O [B, C, W]
  solveFP 14   [O, L]
  solve   14 U [O, L]
  solve   15 M [O, I]
  solve   16 O [S, I]

  putStrLn "Chapter 18"
  solve 1 I [S, K]
  solve 2 M [S, I]
  solve 3 T [S, K, I]
  solve 4 B [S, K, I]

  putStrLn "Chapter 19"
  solve 1 J [E, B, C, W]
  solve 2 (x # (z # y)) [J, I]
  solve 3 T [J, I]
  solve 4 R [J, T]
  solve 5 B [C, J, I]
  solve 6 J1 [J, B, T]
  solve 7 M [J1, T, C]

  putStrLn "Chapter 20"
  solve 1 (z # (x # y)) [G, I]
  solve 2 T [G, I]
  solve 3 C [G, I]
  solve 4 R [C]
  solve 4 Q [R, G, I]
  solve 4 B [C, Q]

  putStrLn "Chapter 21"
  let cs = [B, C, S, I, W, T, M]
  solve 1 (y # x # (x # y # x))         cs
  solve 2 ((z # (y # x)) # (y # x # z)) cs
