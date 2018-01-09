module LibSemigroup where

import Control.Monad
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

--Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  Identity a <> Identity b = Identity a

identityArbitrary :: Arbitrary a => Gen( Identity a )
identityArbitrary = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityArbitrary

--Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

twoArbitrary :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoArbitrary = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoArbitrary

--Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

threeArbitrary :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeArbitrary = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeArbitrary

--Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

fourArbitrary :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourArbitrary = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourArbitrary

--BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj False) = BoolConj False
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) (BoolConj False) _ = BoolConj False

boolConjArbitrary :: Gen BoolConj
boolConjArbitrary = do
  a <- arbitrary
  return (BoolConj a)

instance Arbitrary BoolConj where
  arbitrary = boolConjArbitrary

--BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) (BoolDisj False) (BoolDisj True) = BoolDisj True
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False

boolDisjArbitrary :: Gen BoolDisj
boolDisjArbitrary = do
  a <- arbitrary
  return (BoolDisj a)

instance Arbitrary BoolDisj where
  arbitrary = boolDisjArbitrary

--Or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst a1 <> Fst a2 = Fst a2
  Fst a <> Snd b = Snd b
  Snd b <> Fst a = Snd b
  Snd b1 <> Snd b2 = Snd b1

orArbitrary :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orArbitrary =
  oneof [liftM Fst arbitrary, liftM Snd arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orArbitrary

--Combine
{-
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine aToB1 <> Combine aToB2 = Combine (aToB1 <> aToB2)

combineArbitrary :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
combineArbitrary = do
  aToB <- arbitrary
  return (Combine aToB)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = combineArbitrary
-}

--Comp

--Validation

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success' b1 <> Success' b2 = Success' b1
  Success' b <> Failure' a = Success' b
  Failure' a <> Success' b = Success' b
  Failure' a1 <> Failure' a2 = Failure' (a1 <> a2)

validationArbitrary :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationArbitrary =
  oneof [liftM Failure' arbitrary, liftM Success' arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationArbitrary
