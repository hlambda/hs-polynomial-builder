{-# LANGUAGE FlexibleInstances #-}
module MathObj.Polynomial.Builder (
    Expression(..), Polynomial(..),
    subs, visit, inTermsOf
) where

import MathObj.Polynomial hiding (coeffs)
import Data.Ord (comparing)
import Data.List (find,intersperse,(\\),sort,groupBy)
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
    | Var String
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
    show (Negate x) = "(-(" ++ show x ++ "))"
    show (Exp x n) = "(" ++ show x ++ ")" ++ "^" ++ show n
    show (Var x) = x
    show letter = (:[]) $ fst $ fromJust $ find ((== letter) . snd)
        $ zip ['A'..'Z'] [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

isConst :: Expression a -> Bool
isConst Const{} = True
isConst _ = False

instance Num a => Num (Expression a) where
    (Const 0) + x = x
    x + (Const 0) = x
    (Const x) + (Const y) = Const (x + y)
    (Add xs) + (Add ys) = case (c,filter (not . isConst) zs) of
        (c,[]) -> c
        (Const 0,[x]) -> x
        (c,[x])-> c + x
        (Const 0,fs) -> Add fs
        (c,fs) -> Add $ c : fs
        where
            c = sum $ filter isConst zs
            zs = concatMap g $ xs ++ ys
            g (Add xs) = concatMap g xs
            g x = [x]
    x@Add{} + y = x + Add [y]
    x + y@Add{} = y + Add [x]
    x + y = Add [x,y]
    
    (Const 1) * x = x
    x * (Const 1) = x
    (Const x) * (Const y) = Const (x * y)
    (Mul xs) * (Mul ys) = case (c,filter (not . isConst) zs) of
        (c,[]) -> c
        (Const 0,[x]) -> x
        (c,[x])-> c * x
        (Const 0,fs) -> Mul fs
        (c,fs) -> Mul $ c : fs
        where
            c = product $ filter isConst zs
            zs = concatMap g $ xs ++ ys
            g (Mul xs) = concatMap g xs
            g x = [x]
    x@Mul{} * y = x * Mul [y]
    x * y@Mul{} = y * Mul [x]
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
    deriving Show

-- | Build a polynomial out of an expression
build :: (Num a, Ord a) => Expression a -> Expression a -> Polynomial a
build var (Add as) = Univariate $ map f [ 0 .. maxDeg ] where
    f x = case lookup x terms' of
        Nothing -> 0
        Just x -> x
    terms' = map (first head . second sum . unzip)
        $ groupBy (\x y -> fst x == fst y) $ sort terms
    maxDeg = maximum $ map fst terms
    terms = [ (degree x, coeff x) | x <- as ]
    
    coeff (Const x) = 0
    coeff (Mul ms) = product $ map (\(Const x) -> x) $ filter isConst ms
    coeff _ = 0
    
    degree (Mul ms) = length $ filter (== var) ms
    degree x | x == var = 1
    degree _ = 0

-- | Substitute a sub-expression with a replacement in some expression.
subs :: (Eq a, Ord a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs f r = visit g where
    sf = subexp f
    g e | e == f = r -- term matches
    g e | (not $ null sf) && (null $ sf \\ se) =
        case e of -- all subterms match
            Add xs -> Add $ r : (se \\ sf)
            Mul xs -> Mul $ r : (se \\ sf)
            x -> x -- just a scalar, will be picked up on different visit
        where se = subexp e
    g e = e

-- | Visit all the nodes of an expression, including the root, depth-first.
visit :: (Expression a -> Expression a) -> Expression a -> Expression a
visit f expr = visit' f $ f expr where
    visit' :: (Expression a -> Expression a) -> Expression a -> Expression a
    visit' f (Add xs) = f $ Add $ map (visit' f) xs
    visit' f (Mul xs) = f $ Mul $ map (visit' f) xs
    visit' f (Negate x) = f $ Negate (visit' f x)
    visit' f (Exp x n) = f $ Exp (visit' f x) n
    visit' f x = f x

inTermsOf :: Expression a -> Expression a -> Expression a
inTermsOf var expr = undefined

reduce :: Num a => Expression a -> Expression a
reduce = visit f where
    f (Add xs) = case as ++ bs of
        [x] -> x
        ys -> Add ys
        where
            as = concatMap (\(Add ys) -> ys) $ filter g xs
            bs = filter (not . g) xs
            g Add{} = True
            g _ = False
    f e = e
