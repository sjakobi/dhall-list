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
  , traverseWithIndex
  ) where

-- TODO: Try inlinable instead of inline: some inlinings get pretty huge!

import Control.Applicative (Alternative, liftA2, liftA3)
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

-- TODO: Add Vec constructor (non-empty)
data DhallList a
  = Empty
  | One a
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
  mempty = Empty

instance Functor DhallList where
  fmap = map

instance Applicative DhallList where
  pure = One

  -- TODO: Optimize?
  xs <*> ys = fromList (toList xs <*> toList ys)

instance Monad DhallList where
  -- TODO: Optimize?
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
singleton a = One a

fromList :: [a] -> DhallList a
fromList = \case
  [] -> Empty
  [a] -> One a
  [a, b] -> Many 2 a IEmpty b
  a : as -> Many (Data.Vector.length v + 1) a (ifromVector v') b
    where
      v = Data.Vector.fromList as
      v' = Data.Vector.init v
      b = Data.Vector.last v

fromVector :: Vector a -> DhallList a
fromVector v = case Data.Vector.length v of
  0 -> Empty
  1 -> One (Data.Vector.head v)
  n -> Many n a (ifromVector v') b
    where
      a = Data.Vector.head v
      b = Data.Vector.last v
      v' = Data.Vector.init (Data.Vector.tail v)

replicateM :: Monad m => Int -> m a -> m (DhallList a)
replicateM n0 m = case n0 of
  n | n <= 0 -> pure empty
  1 -> singleton <$> m
  2 -> do
    a <- m
    b <- m
    pure (Many 2 a IEmpty b)
  n -> do
    a <- m
    v <- Data.Vector.replicateM (n - 1) m
    pure (Many n a (IVec (Data.Vector.init v)) (Data.Vector.last v))
{-# inline replicateM #-}

append :: DhallList a -> DhallList a -> DhallList a
append x0 = case x0 of
  Empty -> id
  One x -> \case
    Empty -> x0
    One y -> Many 2 x IEmpty y
    Many sy hy ys ly -> Many (sy + 1) x (icons hy ys) ly
  Many sx hx xs lx -> \case
    Empty -> x0
    One y -> Many (sx + 1) hx (isnoc xs lx) y
    Many sy hy ys ly -> Many (sx + sy) hx (iglue xs lx hy ys) ly
{-# inline append #-}

reverse :: DhallList a -> DhallList a
reverse Empty = Empty
reverse x@One{} = x
reverse (Many s h xs l) = Many s l (ireverse xs) h

length :: DhallList a -> Int
length = \case
  Empty -> 0
  One _ -> 1
  Many s _ _ _ -> s

null :: DhallList a -> Bool
null = \case
  Empty -> True
  _ -> False

head :: DhallList a -> Maybe a
head = \case
  Empty -> Nothing
  One x -> Just x
  Many _ x _ _ -> Just x

last :: DhallList a -> Maybe a
last = \case
  Empty -> Nothing
  One x -> Just x
  Many _ _ _ x -> Just x

foldMap :: Monoid m => (a -> m) -> DhallList a -> m
foldMap f = \case
  Empty -> mempty
  One a -> f a
  Many _ a xs b -> f a <> ifoldMap f xs <> f b
{-# inline foldMap #-}

mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = \case
  Empty -> Empty
  One x -> One (f 0 x)
  Many s a IEmpty b -> Many s (f 0 a) IEmpty (f 1 b)
  Many s a (IVec v) b -> Many s (f 0 a) (IVec (Data.Vector.imap (\ix x -> f (ix + 1) x) v)) (f (s - 1) b)
  Many s a xs b -> Many s (f 0 a) (IVec (Data.Vector.init v1)) (Data.Vector.last v1)
    where
      v1 = Data.Vector.imap (\ix x -> f (ix + 1) x) v0
      v0 = Data.Vector.fromListN (s - 1) (itoList (isnoc xs b))
{-# inline mapWithIndex #-}

map :: (a -> b) -> DhallList a -> DhallList b
map f = \case
  Empty -> Empty
  One x -> One (f x)
  Many s a IEmpty b -> Many s (f a) IEmpty (f b)
  Many s a (IVec v) b -> Many s (f a) (IVec (f <$> v)) (f b)
  Many s a xs b -> Many s (f a) (IVec (Data.Vector.init v)) (Data.Vector.last v)
    where
      v = Data.Vector.fromListN (s - 1) (f <$> itoList (isnoc xs b))
{-# inline map #-}

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> DhallList a -> f (DhallList b)
traverseWithIndex f = \case
  Empty -> pure Empty
  One a -> One <$> f 0 a
  Many s h xs l ->
    liftA3
      (Many s)
      (f 0 h)
      (itraverseWithIndex f 1 xs)
      (f (s - 1) l)

traverse :: Applicative f => (a -> f b) -> DhallList a -> f (DhallList b)
traverse f = traverseWithIndex (\_ix x -> f x)

toList :: DhallList a -> [a]
toList = \case
  Empty -> []
  One a -> [a]
  Many _ h xs l -> h : itoList (isnoc xs l) -- TODO: have itoList :: Inner a -> a -> [a] instead

-- TODO: Optimize me
toVector :: DhallList a -> Vector a
toVector = \case
  Empty -> Data.Vector.empty
  One a -> Data.Vector.singleton a
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

-- TODO: Actually Dhall just needs a mapMWithIndex_ that can be built on top of
-- Data.Vector.imapM_
itraverseWithIndex :: Applicative f => (Int -> a -> f b) -> Int -> Inner a -> f (Inner b)
itraverseWithIndex f ix xs0 = ifromList <$> go (itoList xs0) ix
  where
    go [] _ = pure []
    go (x:xs) n = liftA2 (:) (f n x) (go xs $! n + 1)

ifromList :: [a] -> Inner a
ifromList = \case
  [] -> IEmpty
  [x] -> ICons x IEmpty
  xs -> IVec (Data.Vector.fromList xs)

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
