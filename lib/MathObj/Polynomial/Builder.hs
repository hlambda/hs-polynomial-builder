module MathObj.Polynomial.Builder (
    Expression(..), Polynomial(..),
    -- subs, visit, inTermsOf
) where

import MathObj.Polynomial hiding (coeffs,const,negate)
import Data.Ord (comparing)
import Data.List (find,intersperse,(\\),sort,groupBy,sortBy)
import Data.Maybe (fromJust,isJust)
import Control.Arrow (first,second,(***))
import Control.Monad (join)

import Data.Traversable
import Data.Foldable hiding (concatMap,sum,find,product,maximum,concat)
import Data.Monoid (mappend,mempty,mconcat,appEndo)
import Prelude hiding (foldl,foldl1)

data Expression a
    = Const a
    | Add [Expression a]
    | Mul [Expression a]
    | Negate (Expression a)
    | Exp (Expression a) Int
    -- | Variable letters for expression-construction convenience
    | A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    | Var String -- ^ Free-form variable name
    deriving (Eq,Ord)

instance Num a => Show (Expression a) where
    show (Const x) = show x
    show (Add xs) = "(" ++ (concat $ intersperse " + " $ map show xs) ++ ")"
    show (Mul xs) = concat $ intersperse " * " $ map show xs
    show (Negate x) = "-" ++ show x
    show (Exp x n) = "(" ++ show x ++ ")" ++ "^" ++ show n
    show (Var x) = x
    show letter = (:[]) $ fst $ fromJust $ find ((== letter) . snd)
        $ zip ['A'..'Z'] [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

instance Num a => Num (Expression a) where
    (Const 0) + x = x
    x + (Const 0) = x
    (Const x) + (Const y) = Const (x + y)
    (Add xs) + (Add ys) = sum $ xs ++ ys
    (Add xs) + x = Add (x:xs)
    x + (Add xs) = Add (x:xs)
    x + y = Add [x,y]
    
    -- constant multiplication
    (Const x) * (Const y) = Const (x * y)
    (Const 0) * x = Const 0
    x * (Const 0) = Const 0
    (Const 1) * x = x
    x * (Const 1) = x
    -- expand polynomials in multiplication
    x * (Add xs) = sum $ map (x *) xs
    (Add xs) * x = sum $ map (x *) xs
    -- build recursive product types
    (Mul xs) * (Mul ys) = product $ xs ++ ys
    (Mul xs) * x = Mul (x:xs)
    x * (Mul xs) = Mul (x:xs)
    x * y = Mul [x,y]
    
    -- constant subtraction
    (Const x) - (Const y) = Const (x - y)
    x - y | x == y = Const 0 -- identity zero
    -- distribute negation over addition
    x - (Add xs) = sum $ x : map negate xs
    x - (Negate y) = x + y
    x - y = Add [x,Negate y]
    
    -- the other stuff
    negate (Const x) = Const (-x)
    negate x = Negate x
    
    abs (Const x) = Const (abs x)
    abs x = undefined -- not allowed for now
    
    signum = undefined -- not important enough to implement
    fromInteger = Const . fromInteger

instance Fractional a => Fractional (Expression a) where
    fromRational = Const . fromRational

data Polynomial a = Univariate [a] | Multivariate [(Expression a, [a])]
    deriving Show

-- | Build a polynomial out of an expression
build :: (Num a, Ord a) => Expression a -> Expression a -> Polynomial a
build var (Add xs) = 

-- | Substitute a sub-expression with a replacement in some expression.
subs :: (Eq a, Ord a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs f r = visit g where
    sf = subterms f
    g e | e == f = r -- term matches
    g e | (not $ null sf) && (null $ sf \\ se) =
        case e of -- all subterms match
            Add xs -> Add $ r : (se \\ sf)
            Mul xs -> Mul $ r : (se \\ sf)
            x -> x -- just a scalar, will be picked up on different visit
        where se = subterms e
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

terms :: Expression a -> [Expression a]
terms x = x : (concatMap terms $ subterms x)

leaves :: Expression a -> [Expression a]
leaves = filter ((== 0) . length . subterms) . terms

subterms :: Expression a -> [Expression a]
subterms (Add xs) = xs
subterms (Mul xs) = xs
subterms (Negate x) = [x]
subterms (Exp x _) = [x]
subterms _ = []

{-
inTermsOf :: Expression a -> Expression a -> Expression a
inTermsOf var expr = undefined

classify :: Num a => Expression a -> Expression a -> [(Expression a,Int)]
classify var expr = sortBy (comparing snd)
    $ map (first product . second length . sift (/= var) . f) xs
    where
        (Add xs) = expand expr
        f (Mul xs) = xs
        f x = [x]

sift :: (a -> Bool) -> [a] -> ([a],[a])
sift f xs = (filter f xs, filter (not . f) xs)
-}
