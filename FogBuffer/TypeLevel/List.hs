{-# Language 
 DataKinds
,KindSignatures
,TypeApplications 
,ScopedTypeVariables 
,GADTs
,TypeFamilies
,OverloadedLists
,FlexibleInstances
#-}

module TypeLevel.List where

import GHC.Exts
import TypeLevel.Nat

----
-- List

data List (n :: Nat) a where
 Empty :: List Zero a
 Cons  :: a -> List n a -> List (Succ n) a

instance Show a => Show (List n a) where
 show = show . unList

class ToList (n :: Nat) where
 newList :: [a] -> List n a

instance ToList Zero where
 newList [] = Empty

instance ToList n => ToList (Succ n) where
 newList (x:xs) = x `Cons` newList @n xs

unList :: List n a -> [a]
unList Empty = []
unList (x `Cons` xs) = x : unList xs

instance ToList n => IsList (List n a) where
  type Item (List n a) = a
  toList = unList 
  fromList = newList 

test :: List (ToNat 5) Int
test = [1..5]