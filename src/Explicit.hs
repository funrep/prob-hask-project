{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Explicit where

import System.Random

-- Implementation of the probability distribution monad
-- using Erwig and Kollmansberger (2006) method. This
-- is a discrete implementation and has an inefficient
-- inference strategy. But conveys the ideas how the 
-- Probability monad works which is the base for
-- the monad-bayes library.

type Prob = Double

newtype Explicit a = Explicit { toList :: [(a, Double)]}

instance Functor Explicit where
    fmap f (Explicit xs) = Explicit [(f x, p) | (x, p) <- xs]

instance Applicative Explicit where
    a <*> b = undefined
    pure = return

instance Monad Explicit where
    return x = Explicit [(x, 1)]
    (Explicit xs) >>= f =
        Explicit [ (y, p*q) | (x, p) <- xs
                            , (y, q) <- toList $ f x
                            ]

-- condition takes a function for computing the likelyhood
-- and a distribution, and returns a new conditioned distribution
condition :: (a -> Prob) -> Explicit a -> Explicit a
condition c (Explicit xs) =
    Explicit $ normalize $ reweight c xs

reweight :: Num b => (t -> b) -> [(t, b)] -> [(t, b)]
reweight c xs = map (\(x, p) -> (x, p * c x)) xs

normalize :: Fractional b => [(a, b)] -> [(a, b)]
normalize xs = [(x, p / w) | (x, p) <- xs]
    where
        w = sum $ map snd xs

sample :: StdGen -> Explicit a -> a
sample g (Explicit xs) = scan r xs
    where
        r = fst $ randomR (0.0, 1.0) g
        scan v ((x, p):ps) =
            if v <= p then x else scan (v-p) ps

uniform :: [Integer] -> Explicit Integer
uniform = Explicit . normalize . map (flip (,) 1)

categorical :: [(a, Prob)] -> Explicit a
categorical = Explicit . normalize

bernoulli :: Prob -> Explicit Bool
bernoulli p = categorical [(True,p), (False,1-p)]
