module Util where

-- Utility functions, mainly concerning type level programming.


data Nat = Z | S Nat


-- Singleton type for Nat, used when we require a type level
-- integer at runtime.

data SNat :: Nat -> * where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)


-- Naturals numbers with an upper bound.

data Fin :: Nat -> * where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)


instance Eq (Fin n) where
    x == y = finToInt x == finToInt y


-- A right extending list, indexed by its length.

data RVect :: Nat -> * -> * where
    Nil :: RVect Z a
    Ext :: RVect len a -> a -> RVect (S len) a


instance Functor (RVect len) where
    fmap f Nil        = Nil
    fmap f (Ext xs x) = Ext (fmap f xs) (f x)


finToInt :: Fin n -> Int
finToInt FZ     = 0
finToInt (FS x) = 1 + finToInt x


-- Weaken the index on a Fin without changing its value.

weakenFin :: Fin n -> Fin (S n)
weakenFin FZ     = FZ
weakenFin (FS x) = FS (weakenFin x)


-- Construct the largest Fin possible within the bound.

largest :: SNat n -> Fin (S n)
largest SZ     = FZ
largest (SS n) = FS $ largest n


-- Construct the "opposite" Fin. This might be interpreted as converting
-- between de Bruijn indices and levels.

complement :: SNat n -> Fin n -> Fin n
complement (SS n) FZ     = largest n
complement (SS n) (FS x) = weakenFin $ complement n x


-- Index from the right hand side of the RVect. I.e., the 0th element
-- would be the rightmost one, at the "head" of the list.

rindex :: Fin len -> RVect len a -> a
rindex FZ     (Ext xs x) = x
rindex (FS i) (Ext xs x) = rindex i xs


-- Index from the left hand side of the RVect. I.e., the 0th element
-- would be the leftmost one, at the innermost node of the list.

lindex :: SNat len -> Fin len -> RVect len a -> a
lindex n i xs = rindex (complement n i) xs
