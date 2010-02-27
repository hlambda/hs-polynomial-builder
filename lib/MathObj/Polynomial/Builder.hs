module MathObj.Polynomial.Builder (
    Expression(..), subs
) where

{-
    > subs Z (Var "C.z" - T * Var "D.z")
    $ subs Y (Var "C.y" - T * Var "D.y")
    $ subs X (Var "C.x" - T * Var "D.x")
    $ X^2 + Y^2 + Z^2
    (-T * D.y * C.y + -T * D.y * -T * D.y + C.y * -T * D.y + C.y * C.y + -T * D.z *
    C.z + -T * D.z * -T * D.z + C.z * -T * D.z + C.z * C.z + -T * D.x * -T * D.x +
    -T * D.x * C.x + C.x * C.x + C.x * -T * D.x)
-}

import Data.List (find,intersperse,(\\),sort,group)
import Data.Maybe (fromJust)
import Control.Arrow (first,second,(***),(&&&))

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

newMul :: (Ord a, Num a) => [Expression a] -> Expression a
newMul xs = case first product $ sift f xs of
    (c,[]) -> c
    (1,ys) -> Mul ys
    (c,ys) -> Mul (c:ys)
    where f (Const x) = True; f _ = False

newAdd :: (Ord a, Num a) => [Expression a] -> Expression a
newAdd xs = case first sum $ sift f xs of
    (c,[]) -> c
    (0,ys) -> Add ys
    (c,ys) -> Add (c:ys)
    where f (Const x) = True; f _ = False

-- combine like terms
combine :: (Ord a, Num a) => Expression a -> Expression a
combine (Add xs) = sum
    $ map (uncurry (*) . ((fromIntegral . length) &&& head))
    $ group $ sort xs

instance (Ord a, Num a) => Num (Expression a) where
    -- constant addition rules
    (Const 0) + x = x
    x + (Const 0) = x
    (Const x) + (Const y) = Const (x + y)
    -- roll in addends into a single sum
    (Add xs) + (Add ys) = sum $ xs ++ ys
    (Add xs) + x = newAdd (x:xs)
    x + (Add xs) = newAdd (x:xs)
    x + y = newAdd [x,y]
    
    -- constant multiplication rules
    (Const x) * (Const y) = Const (x * y)
    (Const 0) * x = Const 0
    x * (Const 0) = Const 0
    (Const 1) * x = x
    x * (Const 1) = x
    -- expand polynomials in multiplication
    x * (Add xs) = combine $ sum $ map (x *) xs
    (Add xs) * x = combine $ sum $ map (x *) xs
    -- build recursive product types
    (Mul xs) * (Mul ys) = product $ xs ++ ys
    (Mul xs) * x = newMul (x:xs)
    --x * (Mul xs) = Mul $ (x:xs)
    x * (Mul xs) = newMul (x:xs)
    x * y = newMul [x,y]
    
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

instance (Ord a, Fractional a) => Fractional (Expression a) where
    fromRational = Const . fromRational

-- | Substitute a sub-expression with a replacement in some expression.
-- | Substitution is fairly literal, so substitutions with constants will not
-- | work reliably where reductions can be made.
subs :: (Eq a, Ord a, Num a) =>
    Expression a -> Expression a -> Expression a -> Expression a
subs f r = visit g where
    sf = subterms f
    g e | e == f = r -- term matches
    g e | (not $ null sf) && (null $ sf \\ se) =
        case e of -- all subterms match
            Add{} -> sum $ r : (se \\ sf)
            Mul{} -> product $ r : (se \\ sf)
            x -> x -- just a scalar, will be picked up on different visit
        where se = subterms e
    g e = e

-- only used by substitution since it requires that structures remain the same
visit :: (Ord a, Num a) =>
    (Expression a -> Expression a) -> Expression a -> Expression a
visit f expr = visit' f $ f expr where
    visit' f e@Add{} = sum $ map (visit' f) $ subterms $ f e
    visit' f e@Mul{} = product $ map (visit' f) $ subterms $ f e
    visit' f e@Negate{} = Negate (visit' f x) where Negate x = f e
    visit' f e@Exp{} = Exp (visit' f x) n where Exp x n = f e
    visit' f x = f x

subterms :: Expression a -> [Expression a]
subterms (Add xs) = xs
subterms (Mul xs) = xs
subterms (Negate x) = [x]
subterms (Exp x _) = [x]
subterms _ = []

sift :: (a -> Bool) -> [a] -> ([a],[a])
sift f xs = (filter f xs, filter (not . f) xs)
