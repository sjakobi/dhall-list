{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveLift #-}
{-# language LambdaCase #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-coercions -dsuppress-unfoldings -dsuppress-module-prefixes #-}
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
  , uncons
  , map
  , mapWithIndex
  , mapM_withIndex
  , foldMap
  , foldr'
  , foldl'
  , traverse
  , mapM
  , eqBy
  ) where

import Control.Applicative (Alternative)
import Control.DeepSeq (NFData)
import Control.Monad.ST (ST)
import Data.Coerce (Coercible, coerce)
import Data.Data (Data)
import Data.Monoid (Dual(..))
import Data.Vector (Vector)
import Data.Vector (MVector)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (head, last, length, reverse, null, traverse, map, foldMap, mapM)

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Traversable
import qualified Data.Vector
import qualified Data.Vector.Generic
import qualified Data.Vector.Mutable

data DhallList a
  = Empty
  | One a -- Optimization for List/build
  | Vec {-# unpack #-} !(Vector a)
    -- ^ Invariant: Non-empty
  | Mud {-# unpack #-} !Int a !(Inner a) a
  deriving (Show, Data, Generic, NFData, Lift)

instance Eq a => Eq (DhallList a) where
  (==) = eqBy (==)
  {-# inline (==) #-}

instance Ord a => Ord (DhallList a) where
  -- TODO: Optimize
  compare xs ys = compare (toList xs) (toList ys)
  {-# inline compare #-}

instance Semigroup (DhallList a) where
  (<>) = append

instance Monoid (DhallList a) where
  mempty = empty

instance Functor DhallList where
  fmap = map

instance Applicative DhallList where
  pure = singleton

  -- TODO: Optimize?
  xs <*> ys = fromVector (toVector xs <*> toVector ys)
  {-# inline (<*>) #-}

instance Monad DhallList where
  -- TODO: Optimize?
  xs >>= f = fromVector (toVector xs >>= toVector . f)
  {-# inline (>>=) #-}

instance Alternative DhallList where
  empty = empty
  (<|>) = append

-- TODO: Add foldl (for normalizeWithM)
instance Foldable DhallList where
  foldMap = foldMap
  foldr' = foldr'
  foldl' = foldl'
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
  [] -> empty
  [x] -> singleton x
  xs -> Vec (Data.Vector.fromList xs)
{-# inlinable fromList #-}

-- TODO: I thought this would help vector allocate the right amount of memory,
-- but maybe it doesn't – I should check…
fromListN :: Int -> [a] -> DhallList a
fromListN n xs
  | n <= 0 = empty
  | otherwise = Vec (Data.Vector.fromListN n xs)
{-# inlinable fromListN #-}

fromVector :: Vector a -> DhallList a
fromVector v
  | Data.Vector.null v = Empty
  | otherwise = Vec v
{-# inlinable fromVector #-}

replicateM :: Monad m => Int -> m a -> m (DhallList a)
replicateM n0 m = case n0 of
  n | n <= 0 -> pure empty
  1 -> singleton <$> m
  n -> Vec <$> Data.Vector.replicateM n m
{-# inlinable replicateM #-}

append :: DhallList a -> DhallList a -> DhallList a
append x0 = case x0 of
  Empty -> id
  One x -> \case
    Empty -> x0
    One y -> Vec (Data.Vector.fromListN 2 [x, y]) -- TODO: Check Core
    Vec vy ->
      Mud
        (Data.Vector.length vy + 1)
        x
        (ifromVector (Data.Vector.unsafeInit vy))
        (Data.Vector.unsafeLast vy)
    Mud sy hy ys ly -> Mud (sy + 1) x (icons hy ys) ly
  Vec vx -> \case
    Empty -> x0
    One y ->
      Mud
        (Data.Vector.length vx + 1)
        (Data.Vector.unsafeHead vx)
        (ifromVector (Data.Vector.unsafeTail vx))
        y
    Vec vy ->
      Mud
        (Data.Vector.length vx + Data.Vector.length vy)
        (Data.Vector.unsafeHead vx)
        (icatVecs (Data.Vector.unsafeTail vx) (Data.Vector.unsafeInit vy))
        (Data.Vector.unsafeLast vy)
    Mud sy hy ys ly ->
      Mud
        (Data.Vector.length vx + sy)
        (Data.Vector.unsafeHead vx)
        (ICat (ifromVector (Data.Vector.unsafeTail vx)) (icons hy ys))
        ly
  Mud sx hx xs lx -> \case
    Empty -> x0
    One y -> Mud (sx + 1) hx (isnoc xs lx) y
    Vec vy ->
      Mud
        (sx + Data.Vector.length vy)
        hx
        (ICat (isnoc xs lx) (ifromVector (Data.Vector.unsafeInit vy)))
        (Data.Vector.unsafeLast vy)
    Mud sy hy ys ly -> Mud (sx + sy) hx (iglue xs lx hy ys) ly
{-# inlinable append #-}

eqBy :: (a -> b -> Bool) -> DhallList a -> DhallList b -> Bool
eqBy _ Empty Empty = True
eqBy f xs ys =
      length xs == length ys
      -- TODO: Some more laziness would be nice here…
  &&  Data.Vector.Generic.eqBy f (toVector xs) (toVector ys)
{-# inlinable eqBy #-}

reverse :: DhallList a -> DhallList a
reverse = \case
  Empty -> Empty
  x@One{} -> x
  Vec v -> case Data.Vector.length v of
    1 -> singleton (Data.Vector.unsafeHead v)
    n ->
      Mud
        n
        (Data.Vector.unsafeLast v)
        (ireverse (ifromVector (Data.Vector.unsafeInit (Data.Vector.unsafeTail v))))
        (Data.Vector.unsafeHead v)
  Mud s h xs l -> Mud s l (ireverse xs) h
{-# inlinable reverse #-}

length :: DhallList a -> Int
length = \case
  Empty -> 0
  One _ -> 1
  Vec v -> Data.Vector.length v
  Mud s _ _ _ -> s
{-# inlinable length #-}

null :: DhallList a -> Bool
null = \case
  Empty -> True
  _ -> False
{-# inlinable null #-}

head :: DhallList a -> Maybe a
head = \case
  Empty -> Nothing
  One x -> Just x
  Vec v -> Just (Data.Vector.unsafeHead v)
  Mud _ x _ _ -> Just x
{-# inlinable head #-}

last :: DhallList a -> Maybe a
last = \case
  Empty -> Nothing
  One x -> Just x
  Vec v -> Just (Data.Vector.unsafeLast v)
  Mud _ _ _ x -> Just x
{-# inlinable last #-}

uncons :: DhallList a -> Maybe (a, DhallList a)
uncons = \case
  Empty -> Nothing
  One x -> Just (x, Empty)
  Vec v -> Just (Data.Vector.unsafeHead v, fromVector (Data.Vector.unsafeTail v))
  x@(Mud _ h _ _) -> Just (h, Vec (Data.Vector.unsafeTail (toVector x)))
{-# inlinable uncons #-}

foldMap :: Monoid m => (a -> m) -> DhallList a -> m
foldMap f = \case
  Empty -> mempty
  One a -> f a
  Vec v -> Data.Foldable.foldMap f v
  Mud _ a xs b -> f a <> ifoldMap f xs <> f b
{-# inlinable foldMap #-}

foldr' :: (a -> b -> b) -> b -> DhallList a -> b
foldr' f !y = \case
  Empty -> y
  One x -> f x y
  Vec v -> Data.Vector.foldr' f y v
  Mud _ h xs l -> f h $! ifoldr' f (f l y) xs
{-# inlinable foldr' #-}

foldl' :: (b -> a -> b) -> b -> DhallList a -> b
foldl' f !y = \case
  Empty -> y
  One x -> f y x
  Vec v -> Data.Vector.foldl' f y v
  Mud _ h xs l -> flip f l $! ifoldl' f (f y h) xs
{-# inlinable foldl' #-}

-- | The result is normalized!
map :: (a -> b) -> DhallList a -> DhallList b
map f = \case
  Empty -> Empty
  One x -> One (f x)
  Vec v -> Vec (Data.Vector.map f v)
  x@Mud{} -> Vec (Data.Vector.map f (toVector x))
{-# inlinable map #-}

-- | The result is normalized!
mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = \case
  Empty -> empty
  One x -> singleton (f 0 x)
  Vec v -> Vec (Data.Vector.imap f v)
  x@Mud{} -> Vec (Data.Vector.imap f (toVector x))
{-# inlinable mapWithIndex #-}

-- TODO: Specialize for Either
mapM_withIndex :: Monad m => (Int -> a -> m ()) -> DhallList a -> m ()
mapM_withIndex f = \case
  Empty -> pure ()
  One x -> f 0 x
  Vec v -> Data.Vector.imapM_ f v
  x@Mud{} -> Data.Vector.imapM_ f (toVector x) -- TODO: Avoid the allocation?
{-# inlinable mapM_withIndex #-}

-- | The result is normalized!
traverse :: Applicative f => (a -> f b) -> DhallList a -> f (DhallList b)
traverse f = \case
  Empty -> pure Empty
  One x -> One <$> f x
  Vec v -> Vec <$> Data.Traversable.traverse f v
  x@Mud{} -> Vec <$> Data.Traversable.traverse f (toVector x)
{-# inlinable traverse #-}

-- | More efficient than @traverse@
--
-- The result is normalized.
mapM :: Monad m => (a -> m b) -> DhallList a -> m (DhallList b)
mapM f = \case
  Empty -> pure Empty
  One x -> One <$> f x
  Vec v -> Vec <$> Data.Vector.mapM f v
  x@Mud{} -> Vec <$> Data.Vector.mapM f (toVector x)

-- TODO: Use foldr (:) [] to increase laziness?!
toList :: DhallList a -> [a]
toList = \case
  Empty -> []
  One a -> [a]
  Vec v -> Data.Vector.toList v
  x@Mud{} -> Data.Vector.toList (toVector x)
{-# inlinable toList #-}

toVector :: DhallList a -> Vector a
toVector = \case
  Empty -> Data.Vector.empty
  One a -> Data.Vector.singleton a
  Vec v -> v
  Mud s h xs l -> mudToVector s h xs l
{-# inlinable toVector #-}

mudToVector :: Int -> a -> Inner a -> a -> Vector a
mudToVector n h xs l
  | n == 2 = Data.Vector.fromListN 2 [h, l]
  | otherwise = Data.Vector.create $ do
      v <- Data.Vector.Mutable.unsafeNew n -- Too unsafe?
      Data.Vector.Mutable.unsafeWrite v 0 h
      !ix <- iwrite v 1 xs
      -- assert (ix == n - 1)
      Data.Vector.Mutable.unsafeWrite v ix l
      pure v
{-# inlinable mudToVector #-}

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
{-# inlinable icons #-}

isnoc :: Inner a -> a -> Inner a
isnoc IEmpty y = ICons y IEmpty -- Prefer ICons! Why though?
isnoc (IRev x) y = IRev (ICons y x) -- TODO: Maybe reconsider this optimization
isnoc x y = ISnoc x y
{-# inlinable isnoc #-}

-- TODO: Maybe try some balancing, empty-vector elimination etc. if it seems useful
iglue :: Inner a -> a -> a -> Inner a -> Inner a
iglue as b c ds = case as of
  IEmpty -> b `ICons` (c `ICons` ds)
  _ -> case ds of
    IEmpty -> (as `ISnoc` b) `ISnoc` c
    _ -> as `ICat` (b `ICons` (c `ICons` ds))
{-# inlinable iglue #-}

icatVecs :: Vector a -> Vector a -> Inner a
icatVecs xs ys
  | Data.Vector.null xs = ifromVector ys
  | Data.Vector.null ys = ifromVector xs
  | otherwise = ICat (IVec xs) (IVec ys)
{-# inlinable icatVecs #-}

ireverse :: Inner a -> Inner a
ireverse = \case
  IEmpty  -> IEmpty
  IRev xs -> xs
  xs      -> IRev xs
{-# inlinable ireverse #-}

ifromVector :: Vector a -> Inner a
ifromVector v
  | Data.Vector.null v = IEmpty
  | otherwise = IVec v
{-# inlinable ifromVector #-}

ifoldMap :: Monoid m => (a -> m) -> Inner a -> m
ifoldMap f = \case
  IEmpty -> mempty
  ICons a xs -> f a <> ifoldMap f xs
  ISnoc xs a -> ifoldMap f xs <> f a
  IVec v -> Data.Foldable.foldMap f v
  IRev xs -> getDual (ifoldMap (Dual #. f) xs)
  ICat xs ys -> ifoldMap f xs <> ifoldMap f ys
{-# inlinable ifoldMap #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

ifoldr' :: (a -> b -> b) -> b -> Inner a -> b
ifoldr' f !y = \case
  IEmpty -> y
  ICons x xs -> f x $! ifoldr' f y xs
  ISnoc xs x -> ifoldr' f (f x y) xs
  IVec v -> Data.Vector.foldr' f y v
  IRev xs -> ifoldl' (flip f) y xs
  ICat xs0 xs1 -> ifoldr' f (ifoldr' f y xs1) xs0
{-# inlinable ifoldr' #-}

ifoldl' :: (b -> a -> b) -> b -> Inner a -> b
ifoldl' f !y = \case
  IEmpty -> y
  ICons x xs -> ifoldl' f (f y x) xs
  ISnoc xs x -> flip f x $! ifoldl' f y xs
  IVec v -> Data.Vector.foldl' f y v
  IRev xs -> ifoldr' (flip f) y xs
  ICat xs0 xs1 -> ifoldl' f (ifoldl' f y xs0) xs1
{-# inlinable ifoldl' #-}

-- TODO: Prevent reboxing the Int somehow!?
iwrite :: MVector s a -> Int -> Inner a -> ST s Int
iwrite !mv !ix = \case
  IEmpty -> pure ix
  ICons x ys -> do
    Data.Vector.Mutable.unsafeWrite mv ix x
    iwrite mv (ix + 1) ys
  ISnoc xs y -> do
    !ix' <- iwrite mv ix xs
    Data.Vector.Mutable.unsafeWrite mv ix' y
    pure $! ix' + 1
  IVec v -> do
    let !n = Data.Vector.length v
    let slice = Data.Vector.Mutable.unsafeSlice ix n mv
    Data.Vector.unsafeCopy slice v
    pure $! ix + n
  IRev xs0 -> case xs0 of
    IEmpty -> pure ix
    ICons x ys -> do
      !ix' <- iwrite mv ix (IRev ys)
      Data.Vector.Mutable.unsafeWrite mv ix' x
      pure $! ix' + 1
    ISnoc xs y -> do
      Data.Vector.Mutable.unsafeWrite mv ix y
      iwrite mv (ix + 1) (IRev xs)
    IVec v -> do
      let !n = Data.Vector.length v
      let slice = Data.Vector.Mutable.unsafeSlice ix n mv
      Data.Vector.unsafeCopy slice (Data.Vector.reverse v)
      pure $! ix + n
    IRev xs -> iwrite mv ix xs
    ICat xs ys -> do
      !ix' <- iwrite mv ix (IRev ys)
      iwrite mv ix' (IRev xs)
  ICat xs ys -> do
    !ix' <- iwrite mv ix xs
    iwrite mv ix' ys
{-# inlinable iwrite #-}
