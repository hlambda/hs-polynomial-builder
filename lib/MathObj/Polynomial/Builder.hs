{-# LANGUAGE FlexibleInstances #-}
module MathObj.Polynomial.Builder (
    Expression(..), Polynomial(..),
    subs, visit
) where

import MathObj.Polynomial hiding (coeffs)
import Data.List (find,intersperse,sort)
import Data.Maybe (fromJust,isJust)

data Expression a
    = Const a
    | Add [Expression a]
    | Mul [Expression a]
    | Negate (Expression a)
    | Exp (Expression a) Int
    | A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    | Variable String
    deriving (Eq,Ord)

instance Num a => Show (Expression a) where
    show (Const x) = show x
    show (Add xs) = "(" ++ (concat $ intersperse " + " $ map show xs) ++ ")"
    show (Mul xs) = "(" ++ (concat $ intersperse " * " $ map show xs) ++ ")"
    show (Negate x) = "-(" ++ show x ++ ")"
    show (Exp x n) = "(" ++ show x ++ ")" ++ "^" ++ show n
    show (Variable x) = x
    show letter = (:[]) $ fst $ fromJust $ find ((== letter) . snd)
        $ zip ['A'..'Z'] [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

instance Num a => Num (Expression a) where
    x + y = Add [x,y]
    x * y = Mul [x,y]
    x - y = Add [x,Negate y]
    negate = Negate
    abs = undefined
    signum = undefined
    fromInteger = Const . fromInteger

instance Fractional a => Fractional (Expression a) where
    fromRational = Const . fromRational

data Polynomial a = Univariate [a] | Multivariate [(Expression a, [a])]

-- | Substitute a sub-expression with a replacement in some expression.
subs :: (Eq a, Ord a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs find replace expr = visit g e where
    (f,r,e) = (orderExp find, orderExp replace, orderExp expr)
    g e | f == e = r
    g e = e

-- | Visit all the nodes of an expression, including the root.
visit :: (Expression a -> Expression a) -> Expression a -> Expression a
visit f expr = visit' f $ f expr where
    visit' :: (Expression a -> Expression a) -> Expression a -> Expression a
    visit' f (Add xs) = Add $ map f xs
    visit' f (Mul xs) = Mul $ map f xs
    visit' f (Negate x) = Negate (f x)
    visit' f (Exp x n) = Exp (f x) n
    visit' f x = f x

reduce :: Expression a -> Expression a
reduce expr = visit f expr where
    f (Add xs) = Add $ concatMap g xs where
        g (Add ys) = concatMap g ys
        g e = [e]
    f e = e

-- order associative operations for internal use
orderExp :: Ord a => Expression a -> Expression a
orderExp = visit f where
    f (Add xs) = Add $ sort xs
    f (Mul xs) = Mul $ sort xs
    f expr = expr
