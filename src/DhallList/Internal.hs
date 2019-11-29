{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
module DhallList.Internal
  ( DhallList(..)
  , empty
  , singleton
  , fromList
  , fromVector
  , append
  , reverse
  , length
  , null
  , head
  , last
  , map
  , mapWithIndex
  , traverse
  , traverseWithIndex
  ) where

import Control.Applicative (liftA2, liftA3)
import Data.Functor.Identity (Identity(..))
import Data.Vector (Vector)
import Prelude hiding (head, last, length, reverse, null, traverse, map)

import qualified Data.Foldable
import qualified Data.Traversable
import qualified Data.Vector

data DhallList a
  = Empty
  | One a
  | Many a (Inner a) a

instance Eq a => Eq (DhallList a) where
  -- TODO: Optimize
  xs == ys = toList xs == toList ys

instance Ord a => Ord (DhallList a) where
  -- TODO: Optimize
  compare xs ys = compare (toList xs) (toList ys)

instance Semigroup (DhallList a) where
  (<>) = append

instance Monoid (DhallList a) where
  mempty = Empty

instance Functor DhallList where
  fmap = map

instance Foldable DhallList where
  toList = toList
  length = length
  null = null

instance Traversable DhallList where
  traverse = traverse

empty :: DhallList a
empty = Empty

singleton :: a -> DhallList a
singleton a = One a

fromList :: [a] -> DhallList a
fromList = \case
  [] -> Empty
  [a] -> One a
  [a, b] -> Many a IEmpty b
  a : as -> Many a (ifromVector v') b
    where
      v = Data.Vector.fromList as
      v' = Data.Vector.init v
      b = Data.Vector.last v

fromVector :: Vector a -> DhallList a
fromVector v = case Data.Vector.length v of
  0 -> Empty
  1 -> One (Data.Vector.head v)
  _ -> Many a (ifromVector v') b
    where
      a = Data.Vector.head v
      b = Data.Vector.last v
      v' = Data.Vector.init (Data.Vector.tail v)

append :: DhallList a -> DhallList a -> DhallList a
Empty `append` y = y
x@One{} `append` Empty = x
One x `append` One y = Many x IEmpty y
One x `append` Many hy ys ly = Many x (icons hy ys) ly
x@Many{} `append` Empty = x
Many hx xs lx `append` One y = Many hx (isnoc xs lx) y
Many hx xs lx `append` Many hy ys ly = Many hx (iglue xs lx hy ys) ly

reverse :: DhallList a -> DhallList a
reverse Empty = Empty
reverse x@One{} = x
reverse (Many h xs l) = Many l (ireverse xs) h

length :: DhallList a -> Int
length = \case
  Empty -> 0
  One _ -> 1
  Many _ xs _ -> ilength xs + 2

null :: DhallList a -> Bool
null = \case
  Empty -> True
  _ -> False

head :: DhallList a -> Maybe a
head = \case
  Empty -> Nothing
  One x -> Just x
  Many x _ _ -> Just x

last :: DhallList a -> Maybe a
last = \case
  Empty -> Nothing
  One x -> Just x
  Many _ _ x -> Just x

-- TODO: Implement with Data.Vector.imap instead
mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = runIdentity . traverseWithIndex (\ix x -> Identity (f ix x))

map :: (a -> b) -> DhallList a -> DhallList b
map f = mapWithIndex (\_ix x -> f x)

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> DhallList a -> f (DhallList b)
traverseWithIndex f = \case
  Empty -> pure Empty
  One a -> One <$> f 0 a
  Many h xs l ->
    liftA3
      Many
      (f 0 h)
      (itraverseWithIndex f 1 xs)
      (f (ilength xs + 1) l)

traverse :: Applicative f => (a -> f b) -> DhallList a -> f (DhallList b)
traverse f = traverseWithIndex (\_ix x -> f x)

toList :: DhallList a -> [a]
toList = \case
  Empty -> []
  One a -> [a]
  Many h xs l -> h : itoList xs ++ [l] -- FIXME: Go via dlist

data Inner a
  = IEmpty
  | IOne a
  | ICons !Int a (Inner a)
    -- ^ Invariant: length >= 2
  | ISnoc !Int (Inner a) a
    -- ^ Invariant: length >= 2
  | IVec (Vector a)
    -- ^ Invariant: length >= 2
  | IRev (Inner a)
    -- ^ Invariant: length >= 2
    --   Invariant: The inner Inner is not an IRev itself
  | ICat !Int (Inner a) (Inner a)
    -- ^ Invariant: both inner Inners have length >= 2

icons :: a -> Inner a -> Inner a
icons x IEmpty = IOne x
icons x (IRev y) = IRev (ISnoc (ilength y + 1) y x) -- TODO: Maybe reconsider this optimization
icons x y = ICons (ilength y + 1) x y

isnoc :: Inner a -> a -> Inner a
isnoc IEmpty y = IOne y
isnoc (IRev x) y = IRev (ICons (ilength x + 1) y x) -- TODO: Maybe reconsider this optimization
isnoc x y = ISnoc (ilength x + 1) x y

ilength :: Inner a -> Int
ilength IEmpty = 0
ilength (IOne _) = 1
ilength (ICons s _ _) = s
ilength (ISnoc s _ _) = s
ilength (IVec v) = Data.Vector.length v
ilength (IRev x) = ilength x
ilength (ICat s _ _) = s

iglue :: Inner a -> a -> a -> Inner a -> Inner a
iglue as b c ds = case (as, ds) of
  (IEmpty, IEmpty) -> ICons 2 b (IOne c)
  (IOne a, _) -> ICons (dlen + 3) a (ICons (dlen + 2) b (ICons (dlen + 1) c ds))
    where
      dlen = ilength ds
  _ -> ICat (ilength as + dlen + 2) as (ICons (dlen + 2) b (ICons (dlen + 1) c ds))
    where
      dlen = ilength ds

ireverse :: Inner a -> Inner a
ireverse x0 = case x0 of
  IEmpty -> IEmpty
  IOne _ -> x0
  IRev xs -> xs
  ICat s a b -> ICat s (ireverse b) (ireverse a)
  _      -> IRev x0

itoList :: Inner a -> [a]
itoList = \case
  IEmpty -> []
  IOne a -> [a]
  ICons _ a xs -> a : itoList xs
  ISnoc _ xs a -> itoList xs ++ [a] -- FIXME: Construct a dlist first?
  IVec v -> Data.Vector.toList v
  IRev xs -> ireverseToList xs
  ICat _ xs ys -> itoList xs ++ itoList ys -- FIXME

ireverseToList :: Inner a -> [a]
ireverseToList = \case
  IEmpty -> []
  IOne a -> [a]
  ICons _ a xs -> ireverseToList xs ++ [a] -- FIXME
  ISnoc _ xs a -> a : ireverseToList xs
  IVec v -> Data.Vector.toList (Data.Vector.reverse v)
  IRev xs -> itoList xs
  ICat _ xs ys -> ireverseToList ys ++ ireverseToList xs -- FIXME

itraverseWithIndex :: Applicative f => (Int -> a -> f b) -> Int -> Inner a -> f (Inner b)
itraverseWithIndex f ix xs0 = ifromList <$> go (itoList xs0) ix
  where
    go [] _ = pure []
    go (x:xs) n = liftA2 (:) (f n x) (go xs $! n + 1)

ifromList :: [a] -> Inner a
ifromList = ifromVector . Data.Vector.fromList

ifromVector :: Vector a -> Inner a
ifromVector v = case Data.Vector.length v of
  0 -> IEmpty
  1 -> IOne (Data.Vector.head v)
  _ -> IVec v
