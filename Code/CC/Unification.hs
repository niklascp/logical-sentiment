module Unification where

infix 8 =?

class Unifiable a where
  (=?) :: a -> a -> Bool

instance Unifiable a => Unifiable (Maybe a) where
  Nothing =? _ = False
  _ =? Nothing = False
  Just a =? Just b = a =? b
