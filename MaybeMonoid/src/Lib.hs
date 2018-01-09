module Lib where

import Optional

import Control.Monad (liftM)
import Data.Monoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only a)) = First' (Only a)
  mappend (First' (Only a)) (First' Nada) = First' (Only a)
  mappend (First' (Only a)) (First' (Only b)) = First' (Only a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [(1, return Nada),
               (3, liftM Only arbitrary)]

firstPrimeGen :: Arbitrary a => Gen (First' a)
firstPrimeGen = do
  a <- arbitrary
  frequency [(1, return (First' Nada)),
             (3, return (First' (Only a)))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstPrimeGen

--Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial
  mappend Trivial Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

--Two
data Two a b = Two a b deriving Show

--BoolConj
newtype BoolConj = BoolConj Bool

--Mem
newtype Mem s a =
  Mem { runMem :: s -> (a,s) }
  
instance Monoid a => Monoid (Mem s a) where
  mempty = undefined
  mappend = undefined
