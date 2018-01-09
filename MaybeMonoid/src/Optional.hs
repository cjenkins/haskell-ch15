module Optional where

import Data.Monoid

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend (Only x) Nada = Only x
  mappend Nada (Only y) = Only y
  mappend Nada Nada = Nada
