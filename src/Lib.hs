module Lib where

-- newtype Prob = Prob { toDouble :: Double }
--     deriving (Show, Eq, Ord, Num, Fractional, Real,
--         RealFrac, Floating, Random)

-- prob :: Double -> Prob

-- data Dist a = Dist { pdf :: a -> Double }

-- instance Monad Dist where
--     return = Return
--     (>>=) = Bind

-- uniform :: [a] -> Dist a
-- categorical :: [(a, Prob)] -> Dist a
-- normal :: Double -> Double -> Dist Double
-- beta :: Double -> Double -> Dist Double
-- gamma :: Double -> Double -> Dist Double

-- condition :: (a -> Prob) -> Dist a -> Dist a
-- sample :: StdGen -> Dist a -> a

-- die :: Int -> Dist Int
-- die 0 = return 0
-- die 1 = uniform [1..6]
-- die n = liftM2 (+) (die 1) (die (n-1))

-- result = sample g (die n)
-- results = sample g $ sequence $ replicate k $ die n


