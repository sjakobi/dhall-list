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

data DhallList a
  = Empty
  | One a
  | Many a !(Inner a) a
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

replicateM :: Monad m => Int -> m a -> m (DhallList a)
replicateM n m = case n of
  _ | n <= 0 -> pure empty
  1 -> singleton <$> m
  2 -> do
    a <- m
    b <- m
    pure (Many a IEmpty b)
  3 -> do
    a <- m
    b <- m
    c <- m
    pure (Many a (IOne b) c)
  _ -> do
    a <- m
    v <- Data.Vector.replicateM (n - 1) m
    pure (Many a (IVec (Data.Vector.init v)) (Data.Vector.last v))
{-# inline replicateM #-}

append :: DhallList a -> DhallList a -> DhallList a
append x0 = case x0 of
  Empty -> id
  One x -> \case
    Empty -> x0
    One y -> Many x IEmpty y
    Many hy ys ly -> Many x (icons hy ys) ly
  Many hx xs lx -> \case
    Empty -> x0
    One y -> Many hx (isnoc xs lx) y
    Many hy ys ly -> Many hx (iglue xs lx hy ys) ly
{-# inline append #-}

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

foldMap :: Monoid m => (a -> m) -> DhallList a -> m
foldMap f = \case
  Empty -> mempty
  One a -> f a
  Many a xs b -> f a <> ifoldMap f xs <> f b
{-# inline foldMap #-}

mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = \case
  Empty -> Empty
  One x -> One (f 0 x)
  Many a IEmpty b -> Many (f 0 a) IEmpty (f 1 b)
  Many a (IOne x) b -> Many (f 0 a) (IOne (f 1 x)) (f 2 b)
  Many a (IVec v) b -> Many (f 0 a) (IVec (Data.Vector.imap (\ix x -> f (ix + 1) x) v)) (f (Data.Vector.length v + 1) b)
  Many a xs b -> Many (f 0 a) (IVec (Data.Vector.init v1)) (Data.Vector.last v1)
    where
      v1 = Data.Vector.imap (\ix x -> f (ix + 1) x) v0
      v0 = Data.Vector.fromListN (ilength xs + 1) (itoList (isnoc xs b))
{-# inline mapWithIndex #-}

map :: (a -> b) -> DhallList a -> DhallList b
map f = \case
  Empty -> Empty
  One x -> One (f x)
  Many a IEmpty b -> Many (f a) IEmpty (f b)
  Many a (IOne x) b -> Many (f a) (IOne (f x)) (f b)
  Many a (IVec v) b -> Many (f a) (IVec (f <$> v)) (f b)
  Many a xs b -> Many (f a) (IVec (Data.Vector.init v)) (Data.Vector.last v)
    where
      v = Data.Vector.fromListN (ilength xs + 1) (f <$> itoList (isnoc xs b))
{-# inline map #-}

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
  Many h xs l -> h : itoList (isnoc xs l) -- TODO: have itoList :: Inner a -> a -> [a] instead

toVector :: DhallList a -> Vector a
toVector = \case
  Empty -> Data.Vector.empty
  One a -> Data.Vector.singleton a
  x@Many{} -> Data.Vector.fromListN (length x) (toList x)

-- TODO: Consider having IRev contain a vector, to simplify folds and traversals
-- Or: Remove IRev, implement ireverse via mutable vector

-- TODO: Remove the size fields, track the size in @Many@ instead.
data Inner a
  = IEmpty
  | IOne a -- TODO: Consider removing this constructor
  | ICons {-# unpack #-} !Int a !(Inner a)
    -- ^ Invariant: length >= 2
  | ISnoc {-# unpack #-} !Int !(Inner a) a
    -- ^ Invariant: length >= 2
  | IVec {-# unpack #-} !(Vector a)
    -- ^ Invariant: length >= 2
  | IRev {-# unpack #-} !Int !(Inner a)
    -- ^ Invariant: length >= 2
    --   Invariant: The inner Inner is not an IRev itself
  | ICat {-# unpack #-} !Int !(Inner a) !(Inner a)
    -- ^ Invariant: both inner Inners have length >= 2
  deriving (Show, Data, Generic, NFData, Lift)

icons :: a -> Inner a -> Inner a
icons x IEmpty = IOne x
icons x (IRev s y) = IRev (s + 1) (ISnoc (ilength y + 1) y x) -- TODO: Maybe reconsider this optimization
icons x y = ICons (ilength y + 1) x y
{-# inline icons #-}

isnoc :: Inner a -> a -> Inner a
isnoc IEmpty y = IOne y
isnoc (IRev s x) y = IRev (s + 1) (ICons (ilength x + 1) y x) -- TODO: Maybe reconsider this optimization
isnoc x y = ISnoc (ilength x + 1) x y
{-# inline isnoc #-}

ilength :: Inner a -> Int
ilength IEmpty = 0
ilength (IOne _) = 1
ilength (ICons s _ _) = s
ilength (ISnoc s _ _) = s
ilength (IVec v) = Data.Vector.length v
ilength (IRev s _) = s
ilength (ICat s _ _) = s
{-# inline ilength #-}

-- TODO: Do exhaustive case analysis
iglue :: Inner a -> a -> a -> Inner a -> Inner a
iglue as b c ds = case (as, ds) of
  (IEmpty, IEmpty) -> ICons 2 b (IOne c)
  (IOne a, _) -> ICons (dlen + 3) a (ICons (dlen + 2) b (ICons (dlen + 1) c ds))
    where
      dlen = ilength ds
  _ -> ICat (ilength as + dlen + 2) as (ICons (dlen + 2) b (ICons (dlen + 1) c ds))
    where
      dlen = ilength ds
{-# inline iglue #-}

ireverse :: Inner a -> Inner a
ireverse x0 = case x0 of
  IEmpty -> IEmpty
  IOne _ -> x0
  IRev _ xs -> xs
  _      -> IRev (ilength x0) x0

-- TODO: Consider writing to a mutable vector
itoList :: Inner a -> [a]
itoList = \case
  IEmpty -> []
  IOne a -> [a]
  ICons _ a xs -> a : itoList xs
  ISnoc _ xs a -> itoList xs ++ [a] -- FIXME: Construct a dlist first?
  IVec v -> Data.Vector.toList v
  IRev _ xs -> ireverseToList xs
  ICat _ xs ys -> itoList xs ++ itoList ys -- FIXME
{-# inline itoList #-}

ireverseToList :: Inner a -> [a]
ireverseToList = \case
  IEmpty -> []
  IOne a -> [a]
  ICons _ a xs -> ireverseToList xs ++ [a] -- FIXME
  ISnoc _ xs a -> a : ireverseToList xs
  IVec v -> Data.Vector.toList (Data.Vector.reverse v)
  IRev _ xs -> itoList xs
  ICat _ xs ys -> ireverseToList ys ++ ireverseToList xs -- FIXME

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
  [x] -> IOne x
  xs -> IVec (Data.Vector.fromList xs)

ifromVector :: Vector a -> Inner a
ifromVector v = case Data.Vector.length v of
  0 -> IEmpty
  1 -> IOne (Data.Vector.head v)
  _ -> IVec v

ifoldMap :: Monoid m => (a -> m) -> Inner a -> m
ifoldMap f = \case
  IEmpty -> mempty
  IOne a -> f a
  ICons _ a xs -> f a <> ifoldMap f xs
  ISnoc _ xs a -> ifoldMap f xs <> f a
  IVec v -> Data.Foldable.foldMap f v
  IRev _ xs -> getDual (ifoldMap (Dual #. f) xs)
  ICat _ xs ys -> ifoldMap f xs <> ifoldMap f ys
{-# inline ifoldMap #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
