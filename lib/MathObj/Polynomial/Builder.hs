{-# LANGUAGE FlexibleInstances #-}
module MathObj.Polynomial.Builder (
    Expression(..), Polynomial(..),
    subs, visit
) where

import MathObj.Polynomial hiding (coeffs)
import Data.Ord (comparing)
import Data.List (find,intersperse,(\\),sortBy)
import Data.Maybe (fromJust,isJust)
import Control.Arrow (first,second,(***))
import Control.Monad (join)

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

subexp :: Expression a -> [Expression a]
subexp (Add xs) = xs
subexp (Mul xs) = xs
subexp (Negate x) = [x]
subexp (Exp x _) = [x]
subexp _ = []

instance Num a => Show (Expression a) where
    show (Const x) = show x
    show (Add xs) = "(" ++ (concat $ intersperse " + " $ map show xs) ++ ")"
    show (Mul xs) = "(" ++ (concat $ intersperse " * " $ map show xs) ++ ")"
    show (Negate x) = "-(" ++ show x ++ ")"
    show (Exp x n) = "(" ++ show x ++ ")" ++ "^" ++ show n
    show (Variable x) = x
    show letter = (:[]) $ fst $ fromJust $ find ((== letter) . snd)
        $ zip ['A'..'Z'] [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

isConst :: Expression a -> Bool
isConst Const{} = True
isConst _ = False

instance Num a => Num (Expression a) where
    (Const 0) + x = x
    x + (Const 0) = x
    (Const x) + (Const y) = Const (x + y)
    x@Const{} + y@Add{} = y + Add [x]
    x@Add{} + y@Const{} = x + Add [y]
    (Add xs) + (Add ys) = Add $ c : filter (not . isConst) zs where
        c = sum $ filter isConst zs
        zs = concatMap g $ xs ++ ys
        g (Add xs) = concatMap g xs
        g x = [x]
    x + y = Add [x,y]
    
    (Const 1) * x = x
    x * (Const 1) = x
    (Const x) * (Const y) = Const (x * y)
    x@Const{} * y@Mul{} = y * Mul [x]
    x@Mul{} * y@Const{} = x * Mul [y]
    (Mul xs) * (Mul ys) = Mul $ c : filter (not . isConst) zs where
        c = product $ filter isConst zs
        zs = concatMap g $ xs ++ ys
        g (Mul xs) = concatMap g xs
        g x = [x]
    x * y = Mul [x,y]
    
    (Const x) - (Const y) = Const (x - y)
    x - y = Add [x,Negate y]
    
    negate = Negate
    abs = undefined
    signum = undefined
    fromInteger = Const . fromInteger

instance Fractional a => Fractional (Expression a) where
    fromRational = Const . fromRational

data Polynomial a = Univariate [a] | Multivariate [(Expression a, [a])]

-- | Build a polynomial out of an expression
build :: Expression a -> Polynomial a
build expr = undefined

-- | Substitute a sub-expression with a replacement in some expression.
subs :: (Eq a, Ord a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs f r = visit g where
    sf = subexp f
    g e | e == f = r -- term matches
    g e | null $ sf \\ (subexp e) = case e of -- all subterms match
        Add xs -> Add $ r : ((subexp e) \\ sf)
        Mul xs -> Mul $ r : ((subexp e) \\ sf)
        x -> x -- just a scalar, will be picked up on different visit
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
