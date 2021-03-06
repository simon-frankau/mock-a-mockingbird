module Combinator where

import Control.Arrow((&&&))
import Control.Monad(forM_)

import qualified Data.Set as Set

data Expr = Ap Expr Expr
          | L
            deriving (Eq, Ord)

instance Show Expr where
  show (Ap e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show L = "L"

exprOfSize :: Integer -> [Expr]
exprOfSize 0 = []
exprOfSize 1 = [L]
exprOfSize n = concatMap aux [1..n - 1] where
  aux i = Ap <$> exprOfSize i <*> exprOfSize (n - i)

multiStep :: Expr -> [Expr]
multiStep e = top ++ rec where
  top = case e of
    Ap (Ap L x) y -> [Ap x (Ap y y)]
    _ -> []
  rec = case e of
    (Ap x y) -> ((\x -> Ap x y) <$> (multiStep x)) ++
                ((\y -> Ap x y) <$> (multiStep y))
    _ -> []

boundedMultiSteps :: Integer -> Expr -> Set.Set Expr
boundedMultiSteps i x = aux i $ Set.fromList [x] where
  aux 0 s = s
  aux i s = aux (i - 1) s' where
    s' = Set.fromList $ concatMap multiStep $ Set.toList s

boundedMultiIsEgoCentric :: Integer -> Expr -> Bool
boundedMultiIsEgoCentric i x = -- not $ Set.disjoint xs xxs where
                               not $ Set.null $ Set.intersection xs xxs where
  xs  = boundedMultiSteps i x
  xxs = boundedMultiSteps i (Ap x x)

egoMultiCheckOfSize i = (id &&& boundedMultiIsEgoCentric 5) <$> exprOfSize i

main = do
  forM_ [1..12] $ \i -> do
    print i
    mapM_ print $ filter snd $ egoMultiCheckOfSize i
