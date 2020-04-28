module ReverseAD where

-- Reverse automatic differentiation for one dimension
-- based on the code from https://github.com/jonnylaw/prob-programming-examples
-- which is implementation of a paper
-- by Law and Wilkinson (2019).

data Reverse a = Reverse a (Double -> Double)

instance Functor Reverse where
    fmap f (Reverse v c) = Reverse (f v) c

instance Applicative Reverse where
    (Reverse f c) <*> (Reverse v' c') = Reverse (f v') (c . c')
    pure a = Reverse a id 

instance Monad Reverse where
    (Reverse v c) >>= f =
        let (Reverse v' c') = f v
        in Reverse v' $ \x -> c $ c' 1 * x

mul :: Reverse Double -> Reverse Double -> Reverse Double
mul (Reverse v c) (Reverse v' c') =
    Reverse (v * v') $ \x -> c (x * v') + c' (x * v)

div :: Reverse Double -> Reverse Double -> Reverse Double
div (Reverse v c) (Reverse v' c') =
    Reverse (v / v') $ \x -> (c (x * v') - (c' (x * v))) / (v'**2)

add :: Reverse Double -> Reverse Double -> Reverse Double
add (Reverse v c) (Reverse v' c') =
    Reverse (v + v') $ \x -> c x + c' x

sub :: Reverse Double -> Reverse Double -> Reverse Double
sub (Reverse v c) (Reverse v' c') =
    Reverse (v - v') $ \x -> c x - c' x

pow :: Reverse Double -> Double -> Reverse Double
pow a r =
    a >>=
    (\x -> Reverse (x**r) $ \_ -> r * x**(r - 1))

