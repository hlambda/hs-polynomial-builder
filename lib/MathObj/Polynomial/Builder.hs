{-# LANGUAGE FlexibleInstances #-}
module MathObj.Polynomial.Builder (
    Expression(..), Polynomial(..),
    subs, visit
) where

import MathObj.Polynomial hiding (coeffs)
import Data.List (intersperse)

data Expression a
    = Const a
    | Add (Expression a) (Expression a)
    | Sub (Expression a) (Expression a)
    | Negate (Expression a)
    | Mul (Expression a) (Expression a)
    | Exp (Expression a) Int
    | X | Y | Z | W
    | Variable String
    deriving (Eq,Ord)

instance Num a => Show (Expression a) where
    show (Const x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Negate x) = "-(" ++ show x ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Exp x n) = "(" ++ show x ++ ")" ++ "^" ++ show n
    show X = "X"
    show Y = "Y"
    show Z = "Z"
    show W = "W"
    show (Variable x) = x

instance Num a => Num (Expression a) where
    (+) = Add
    (*) = Mul
    abs = Negate
    signum = undefined
    fromInteger = Const . fromInteger

instance Fractional a => Fractional (Expression a) where
    fromRational = Const . fromRational

data Polynomial a = Univariate [a] | Multivariate [(Expression a, [a])]

-- | Substitute a sub-expression with a replacement in some expression.
subs :: (Eq a, Ord a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs find replace expr = subs' f r e where
    (f,r,e) = (orderExp find, orderExp replace, orderExp expr)
    subs' f r expr | f == expr = r
    subs' f r (Add x y) = Add (subs' f r x) (subs' f r y)
    subs' f r (Mul x y) = Mul (subs' f r x) (subs' f r y)
    subs' _ _ expr = expr

-- order associative operations for internal use
orderExp :: Ord a => Expression a -> Expression a
orderExp = visit f where
    f (Mul x y) = Mul (min x y) (max x y)
    f (Add x y) = Add (min x y) (max x y)
    f expr = expr

-- | Visit all the nodes of an expression, including the root.
visit :: (Expression a -> Expression a) -> Expression a -> Expression a
visit f expr = visit' f $ f expr where
    visit' :: (Expression a -> Expression a) -> Expression a -> Expression a
    visit' f (Add x y) = Add (f x) (f y)
    visit' f (Sub x y) = Sub (f x) (f y)
    visit' f (Mul x y) = Mul (f x) (f y)
    visit' f (Negate x) = Negate (f x)
    visit' f (Exp x n) = Exp (f x) n
    visit' f x = f x
