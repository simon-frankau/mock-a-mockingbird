-- Algorithm to build arbitrary compositors.
--
-- Simon Frankau (sgf@arbitrary.name), 2018

module Bluebird where

import Control.Monad(forM_)

data Expr = Ap Expr Expr
          | B
          | X
            deriving (Eq, Ord)

instance Show Expr where
  show e = show' False e

show' needBracket (Ap e1 e2) =
  let e = show' False e1 ++ " " ++ show' True e2
  in if needBracket then"(" ++ e ++ ")" else e
show' _ B = "B"
show' _ X = "X"

rotatify :: Expr -> Expr
rotatify e
  | containsX e = rotatify $ rot1 e
  | otherwise   = e
  where
    -- Variables are always on the right
    containsX (Ap _ e) = containsX e
    containsX B = False
    containsX X = True
    -- If there's a variable on the right of the root, it's done.
    rot1 (Ap e X) = e
    -- Otherwise rotate the root
    rot1 (Ap e1 (Ap e2 e3)) = Ap (Ap (Ap B e1) e2) e3
    -- If we've rotated everything away, it's the identity function
    rot1 X = error "Nothing to do"
    -- And shapes that shouldn't happen
    rot1 B        = error "containsX failure"
    rot1 (Ap e B) = error "containsX failure (2)"

a # b = Ap a b

cases :: [Expr]
cases = [
    X # (X # X),                  -- B  bluebird
    X # X # (X # X),              -- D  dove
    X # (X # X # X),              -- B1 blackbird
    X # X # (X # X # X),          -- E  eagle
    X # (X # X # X # X),          -- B2 bunting
    X # X # X # (X # X),          -- D1 dickcissel
    X # (X # (X # X)),            -- B3 becard
    X # (X # X) # (X # X),        -- D2 dovekie
    X # (X # X # X) # (X # X # X) -- E' bald eagle
  ]

main = do
  forM_ cases $ \e -> do
    let e' = rotatify e
    putStrLn $ show e ++ " -> " ++ show e'
