{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
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


module FogBuffer (
    FogBuffer,
    new, fromList, size, push, get
) where

import Data.Proxy
import Control.Monad.ST
import qualified Data.Array.ST as A
import Data.STRef
import TypeLevel.List
import TypeLevel.Nat
import TypeLevel.BoundedInt

-- The Int is the start of the virtual view on the array. A push
-- shifts this position one to the left and writes the new value there,
-- so that a FogBuffer acts like a cons-list where all values past some
-- point into the list are "forgotten".
data FogBuffer (n :: Nat) s a =
     FogBuffer { _fbLength :: Int
               , _fbHead :: STRef s Int
               , _fbArr :: A.STArray s Int a }

-- | Construct a new buffer from a length and an initial value for each
-- position.

new :: forall n s a. IsNat n => a -> ST s (FogBuffer n s a)
new defVal = let len = fromNat (getNat (Proxy @n)) in
    FogBuffer len <$> newSTRef 0 <*> A.newArray (0, len - 1) defVal

initialize :: [a] -> ST s (FogBuffer n s a)
initialize initVals =
    let len = length initVals
    in FogBuffer len <$> newSTRef 0 <*> A.newListArray (0, len - 1) initVals

fromList :: List n a -> ST s (FogBuffer n s a)
fromList = initialize . unList

-- | Get the length the buffer was initialised with.
size :: FogBuffer n s a -> Int
size (FogBuffer len _ _) = len

-- | Push a new value in front, and forget an old value.
push :: FogBuffer n s a -> a -> ST s a
push (FogBuffer len ptrref arr) value = do
    ptr <- readSTRef ptrref
    let newPtr = if ptr == 0 then len - 1 else ptr - 1
    oldvalue <- A.readArray arr newPtr
    A.writeArray arr newPtr value
    writeSTRef ptrref newPtr
    return oldvalue

-- | Assumes the index is in the range [0, length - 1]; accesses a value
-- in the virtual view on the array.
get :: FogBuffer n s a -> BoundedInt n -> ST s a
get (FogBuffer len ptrref arr) (BoundedInt idx) = do
    ptr <- readSTRef ptrref
    let idx' = ptr + idx
    A.readArray arr (if idx' >= len then idx' - len else idx')

test' :: IO ()
test' = print $ runST $ do
    buf <- fromList ([4,3,2,1] :: List (ToNat 4) Double)
    _ <- push buf 5
    old <- push buf 6
    (old,) <$> sequence [get buf i | i <- map (BoundedInt @(ToNat 4)) [0..3]]