module ForwardAD where

-- Forward automatic differentiation
-- based on the code from https://github.com/jonnylaw/prob-programming-examples
-- which is implementation of a paper
-- by Law and Wilkinson (2019).

data Dual a = Dual a Double
    deriving (Eq, Show)

instance Functor Dual where
    fmap f (Dual v d) = Dual (f v) d

instance Applicative Dual where
    (Dual f d) <*> (Dual v' d') = Dual (f v') (d * d')
    pure a = Dual a 1.0

instance Monad Dual where
    (Dual v d) >>= f = 
        let Dual v' d' = f v
        in Dual v' $ d * d'

liftConst :: a -> Dual a
liftConst a = Dual a 0.0

mul :: Dual Double -> Dual Double -> Dual Double
(Dual v d) `mul` (Dual v' d') = Dual (v * v') (v * d' + d * v')

div :: Dual Double -> Dual Double -> Dual Double
(Dual v d) `div` (Dual v' d') = Dual (v / v') (d * v' - (d' * v) / (v'**2))

add :: Dual Double -> Dual Double -> Dual Double
(Dual v d) `add` (Dual v' d') = Dual (v + v') (d + d')

sub :: Dual Double -> Dual Double -> Dual Double
(Dual v d) `sub` (Dual v' d') = Dual (v - v') (d - d')

pow :: Dual Double -> Double -> Dual Double
pow (Dual v d) k = Dual (v**k) (k * v**(k - 1))

sinD :: Dual Double -> Dual Double
sinD = (=<<) (\v -> Dual (sin v) (cos v))

cosD :: Dual Double -> Dual Double
cosD = (=<<) (\v -> Dual (cos v) (sin v * (-1)))

tanD :: Dual Double -> Dual Double
tanD = (=<<) (\v -> Dual (tan v) (1 + (tan v)**2))

expD :: Dual Double -> Dual Double
expD = (=<<) (\v -> Dual (exp v) (1 + (exp v)**2))
