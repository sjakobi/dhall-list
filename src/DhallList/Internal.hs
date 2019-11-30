{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveLift #-}
{-# language LambdaCase #-}
module DhallList.Internal
  ( DhallList(..)
  , empty
  , singleton
  , fromList
  , fromListN
  , fromVector
  , replicateM
  , toList
  , toVector
  , append
  , reverse
  , length
  , null
  , head
  , last
  , map
  , mapWithIndex
  , traverse
  ) where

-- TODO: Try inlinable instead of inline: some inlinings get pretty huge!
-- TODO: Use unsafe Vector operations
-- TODO: Optimize Inner operations based on size of Many
-- TODO: Add a mapMWithIndex_ or mapM_withIndex that can be built on top of Data.Vector.imapM_

import Control.Applicative (Alternative)
import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.Data (Data)
import Data.Monoid (Dual(..))
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (head, last, length, reverse, null, traverse, map, foldMap)

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Traversable
import qualified Data.Vector

data DhallList a
  = Empty
  | One a -- TODO: Consider removing
  | Vec {-# unpack #-} !(Vector a)
    -- ^ Invariant: Non-empty
  | Many {-# unpack #-} !Int a !(Inner a) a
  deriving (Show, Data, Generic, NFData, Lift)

instance Eq a => Eq (DhallList a) where
  -- TODO: Optimize
  -- TODO: Define as @eqBy (==)@
  xs == ys = toList xs == toList ys

instance Ord a => Ord (DhallList a) where
  -- TODO: Optimize
  compare xs ys = compare (toList xs) (toList ys)

instance Semigroup (DhallList a) where
  (<>) = append

instance Monoid (DhallList a) where
  mempty = empty

instance Functor DhallList where
  fmap = map

instance Applicative DhallList where
  pure = singleton

  -- TODO: Optimize?
  xs <*> ys = fromListN (length xs * length ys) (toList xs <*> toList ys)

instance Monad DhallList where
  -- TODO: Optimize? Use Vector instead of list?
  xs >>= f = fromList (toList xs >>= toList . f)

instance Alternative DhallList where
  empty = empty
  (<|>) = append

-- TODO: Add foldr' (needed for Dhall.Eval)
instance Foldable DhallList where
  foldMap = foldMap
  toList = toList
  length = length
  null = null

instance Traversable DhallList where
  traverse = traverse

empty :: DhallList a
empty = Empty

singleton :: a -> DhallList a
singleton = One

fromList :: [a] -> DhallList a
fromList = \case
  [] -> Empty
  [x] -> One x
  xs -> Vec (Data.Vector.fromList xs)

fromListN :: Int -> [a] -> DhallList a
fromListN n xs
  | n <= 0 = empty
  | otherwise = Vec (Data.Vector.fromListN n xs)

fromVector :: Vector a -> DhallList a
fromVector v
  | Data.Vector.null v = Empty
  | otherwise = Vec v

replicateM :: Monad m => Int -> m a -> m (DhallList a)
replicateM n0 m = case n0 of
  n | n <= 0 -> pure empty
  1 -> singleton <$> m
  n -> Vec <$> Data.Vector.replicateM n m
{-# inline replicateM #-}

append :: DhallList a -> DhallList a -> DhallList a
append x0 = case x0 of
  Empty -> id
  One x -> \case
    Empty -> x0
    One y -> Vec (Data.Vector.fromListN 2 [x, y]) -- TODO: Check Core
    Vec vy ->
      Many
        (Data.Vector.length vy + 1)
        x
        (ifromVector (Data.Vector.init vy))
        (Data.Vector.last vy)
    Many sy hy ys ly -> Many (sy + 1) x (icons hy ys) ly
  Vec vx -> \case
    Empty -> x0
    One y ->
      Many
        (Data.Vector.length vx + 1)
        (Data.Vector.head vx)
        (ifromVector (Data.Vector.tail vx))
        y
    Vec vy ->
      Many
        (Data.Vector.length vx + Data.Vector.length vy)
        (Data.Vector.head vx)
        (icatVecs (Data.Vector.tail vx) (Data.Vector.init vy))
        (Data.Vector.last vy)
    Many sy hy ys ly ->
      Many
        (Data.Vector.length vx + sy)
        (Data.Vector.head vx)
        (ICat (IVec (Data.Vector.tail vx)) (icons hy ys)) -- TODO: Maybe optimize using sy
        ly
  Many sx hx xs lx -> \case
    Empty -> x0
    One y -> Many (sx + 1) hx (isnoc xs lx) y
    Vec vy ->
      Many
        (sx + Data.Vector.length vy)
        hx
        (ICat (isnoc xs lx) (IVec (Data.Vector.init vy)))
        (Data.Vector.last vy)
    Many sy hy ys ly -> Many (sx + sy) hx (iglue xs lx hy ys) ly
{-# inline append #-}

reverse :: DhallList a -> DhallList a
reverse = \case
  Empty -> Empty
  x@One{} -> x
  Vec v -> case Data.Vector.length v of
    1 -> One (Data.Vector.head v)
    n ->
      Many
        n
        (Data.Vector.head v)
        (ifromVector (Data.Vector.init (Data.Vector.tail v)))
        (Data.Vector.last v)
  Many s h xs l -> Many s l (ireverse xs) h

length :: DhallList a -> Int
length = \case
  Empty -> 0
  One _ -> 1
  Vec v -> Data.Vector.length v
  Many s _ _ _ -> s

null :: DhallList a -> Bool
null = \case
  Empty -> True
  _ -> False

head :: DhallList a -> Maybe a
head = \case
  Empty -> Nothing
  One x -> Just x
  Vec v -> Just (Data.Vector.head v)
  Many _ x _ _ -> Just x

last :: DhallList a -> Maybe a
last = \case
  Empty -> Nothing
  One x -> Just x
  Vec v -> Just (Data.Vector.last v)
  Many _ _ _ x -> Just x

foldMap :: Monoid m => (a -> m) -> DhallList a -> m
foldMap f = \case
  Empty -> mempty
  One a -> f a
  Vec v -> Data.Foldable.foldMap f v
  Many _ a xs b -> f a <> ifoldMap f xs <> f b
{-# inline foldMap #-}

-- | The result is normalized!
mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = \case
  Empty -> Empty
  One x -> One (f 0 x)
  Vec v -> Vec (Data.Vector.imap f v)
  x@Many{} -> Vec (Data.Vector.imap f (toVector x))
{-# inline mapWithIndex #-}

-- | The result is normalized!
map :: (a -> b) -> DhallList a -> DhallList b
map f = \case
  Empty -> Empty
  One x -> One (f x)
  Vec v -> Vec (Data.Vector.map f v)
  x@Many{} -> Vec (Data.Vector.map f (toVector x))
{-# inline map #-}

-- | The result is normalized!
traverse :: Applicative f => (a -> f b) -> DhallList a -> f (DhallList b)
traverse f = \case
  Empty -> pure Empty
  One x -> One <$> f x
  Vec v -> Vec <$> Data.Traversable.traverse f v
  x@Many{} -> Vec <$> Data.Traversable.traverse f (toVector x)

toList :: DhallList a -> [a]
toList = \case
  Empty -> []
  One a -> [a]
  Vec v -> Data.Vector.toList v
  Many _ h xs l -> h : itoList (isnoc xs l) -- TODO: have itoList :: Inner a -> a -> [a] instead

-- TODO: Optimize me
toVector :: DhallList a -> Vector a
toVector = \case
  Empty -> Data.Vector.empty
  One a -> Data.Vector.singleton a
  Vec v -> v
  x@Many{} -> Data.Vector.fromListN (length x) (toList x)

-- TODO: Consider having IRev contain a vector, to simplify folds and traversals
-- Or: Remove IRev, implement ireverse via mutable vector
data Inner a
  = IEmpty
  | ICons a !(Inner a)
  | ISnoc !(Inner a) a
  | IVec {-# unpack #-} !(Vector a)
  | IRev !(Inner a)
  | ICat !(Inner a) !(Inner a)
  deriving (Show, Data, Generic, NFData, Lift)

icons :: a -> Inner a -> Inner a
icons x (IRev y) = IRev (ISnoc y x) -- TODO: Maybe reconsider this optimization
icons x y = ICons x y
{-# inline icons #-}

isnoc :: Inner a -> a -> Inner a
isnoc IEmpty y = ICons y IEmpty -- Prefer ICons! Why though?
isnoc (IRev x) y = IRev (ICons y x) -- TODO: Maybe reconsider this optimization
isnoc x y = ISnoc x y
{-# inline isnoc #-}

-- TODO: Maybe try some balancing, empty-vector elimination etc. if it seems useful
iglue :: Inner a -> a -> a -> Inner a -> Inner a
iglue as b c ds = case as of
  IEmpty -> b `ICons` (c `ICons` ds)
  _ -> case ds of
    IEmpty -> (as `ISnoc` b) `ISnoc` c
    _ -> as `ICat` (b `ICons` (c `ICons` ds))
{-# inline iglue #-}

-- TODO: Consider doing some normalization?!
icatVecs :: Vector a -> Vector a -> Inner a
icatVecs xs ys = ICat (IVec xs) (IVec ys)

ireverse :: Inner a -> Inner a
ireverse x0 = case x0 of
  IEmpty -> IEmpty
  IRev xs -> xs
  _      -> IRev x0

-- TODO: Consider writing to a mutable vector
itoList :: Inner a -> [a]
itoList = \case
  IEmpty -> []
  ICons a xs -> a : itoList xs
  ISnoc xs a -> itoList xs ++ [a] -- FIXME: Construct a dlist first?
  IVec v -> Data.Vector.toList v
  IRev xs -> ireverseToList xs
  ICat xs ys -> itoList xs ++ itoList ys -- FIXME
{-# inline itoList #-}

ireverseToList :: Inner a -> [a]
ireverseToList = \case
  IEmpty -> []
  ICons a xs -> ireverseToList xs ++ [a] -- FIXME
  ISnoc xs a -> a : ireverseToList xs
  IVec v -> Data.Vector.toList (Data.Vector.reverse v)
  IRev xs -> itoList xs
  ICat xs ys -> ireverseToList ys ++ ireverseToList xs -- FIXME

ifromVector :: Vector a -> Inner a
ifromVector = IVec

ifoldMap :: Monoid m => (a -> m) -> Inner a -> m
ifoldMap f = \case
  IEmpty -> mempty
  ICons a xs -> f a <> ifoldMap f xs
  ISnoc xs a -> ifoldMap f xs <> f a
  IVec v -> Data.Foldable.foldMap f v
  IRev xs -> getDual (ifoldMap (Dual #. f) xs)
  ICat xs ys -> ifoldMap f xs <> ifoldMap f ys
{-# inline ifoldMap #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
