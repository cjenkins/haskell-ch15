module Main where

import Data.Semigroup
import Test.QuickCheck

import LibSemigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool
type FourAssoc = Four String String String String -> Four String String String String ->
                 Four String String String String -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc = Or String String -> Or String String -> Or String String -> Bool
--type CombineAssoc = Combine (String->String) -> Combine (String->String) -> Combine (String->String) -> Bool
type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

main :: IO ()
main = do
  quickCheck( semigroupAssoc :: TrivAssoc )
  quickCheck( semigroupAssoc :: IdentityAssoc )
  quickCheck( semigroupAssoc :: TwoAssoc )
  quickCheck( semigroupAssoc :: ThreeAssoc )
  quickCheck( semigroupAssoc :: FourAssoc )
  quickCheck( semigroupAssoc :: BoolConjAssoc )
  quickCheck( semigroupAssoc :: BoolDisjAssoc )
  quickCheck( semigroupAssoc :: OrAssoc )
--  quickCheck( semigroupAssoc :: CombineAssoc )
  quickCheck( semigroupAssoc :: ValidationAssoc )
