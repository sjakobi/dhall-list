{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveLift #-}
{-# language LambdaCase #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-coercions -dsuppress-unfoldings -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -O2 #-}

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
  , foldr
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
import Data.DList (DList)
import Data.Monoid (Dual(..))
import Data.Vector (Vector)
import Data.Vector (MVector)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (head, last, length, reverse, null, traverse, map, foldMap, mapM, foldr)

import qualified Control.Applicative
import qualified Data.DList as DList
import qualified Data.Foldable
import qualified Data.Traversable
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as Vector.Generic
import qualified Data.Vector.Mutable as Vector.Mutable

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

-- TODO: Add (<$)
instance Functor DhallList where
  fmap = map

instance Applicative DhallList where
  pure = singleton

  fs0 <*> as0 = case fs0 of
    Empty -> Empty
    One f -> map f as0
    Vec fs -> case as0 of
      Empty -> Empty
      One a -> Vec (fmap ($ a) fs)
      Vec as -> Vec (fs <*> as)
      as@Mud{} -> Vec (fs <*> toVector as)
    fs@Mud{} -> case as0 of
      Empty -> Empty
      One a -> Vec (fmap ($ a) (toVector fs))
      as -> Vec (toVector fs <*> toVector as)
  {-# inline (<*>) #-}

instance Monad DhallList where
  xs0 >>= f = case xs0 of
    Empty -> Empty
    One x -> f x
    xs -> fromVector (toVector xs >>= toVector . f)
  {-# inline (>>=) #-}

instance Alternative DhallList where
  empty = empty
  (<|>) = append

-- TODO: Add foldl (for normalizeWithM)
instance Foldable DhallList where
  foldMap = foldMap
  foldr = foldr
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
  xs -> Vec (Vector.fromList xs)
{-# inlinable fromList #-}

fromListN :: Int -> [a] -> DhallList a
fromListN n xs
  | n <= 0 = empty
  | otherwise = Vec (Vector.fromListN n xs)
{-# inlinable fromListN #-}

fromVector :: Vector a -> DhallList a
fromVector v
  | Vector.null v = Empty
  | otherwise = Vec v
{-# inlinable fromVector #-}

replicateM :: Monad m => Int -> m a -> m (DhallList a)
replicateM n0 m = case n0 of
  n | n <= 0 -> pure empty
  1 -> singleton <$> m
  n -> Vec <$> Vector.replicateM n m
{-# inlinable replicateM #-}

append :: DhallList a -> DhallList a -> DhallList a
append x0 = case x0 of
  Empty -> id
  One x -> \case
    Empty -> x0
    One y -> Vec (Vector.fromListN 2 [x, y])
    Vec vy ->
      Mud
        (Vector.length vy + 1)
        x
        (ifromVector (Vector.unsafeInit vy))
        (Vector.unsafeLast vy)
    Mud sy hy ys ly -> Mud (sy + 1) x (icons hy ys) ly
  Vec vx -> \case
    Empty -> x0
    One y ->
      Mud
        (Vector.length vx + 1)
        (Vector.unsafeHead vx)
        (ifromVector (Vector.unsafeTail vx))
        y
    Vec vy ->
      Mud
        (Vector.length vx + Vector.length vy)
        (Vector.unsafeHead vx)
        (icatVecs (Vector.unsafeTail vx) (Vector.unsafeInit vy))
        (Vector.unsafeLast vy)
    Mud sy hy ys ly ->
      Mud
        (Vector.length vx + sy)
        (Vector.unsafeHead vx)
        (ICat (ifromVector (Vector.unsafeTail vx)) (icons hy ys))
        ly
  Mud sx hx xs lx -> \case
    Empty -> x0
    One y -> Mud (sx + 1) hx (isnoc xs lx) y
    Vec vy ->
      Mud
        (sx + Vector.length vy)
        hx
        (ICat (isnoc xs lx) (ifromVector (Vector.unsafeInit vy)))
        (Vector.unsafeLast vy)
    Mud sy hy ys ly -> Mud (sx + sy) hx (iglue xs lx hy ys) ly
{-# inlinable append #-}

-- TODO: This could be even more efficient if we had ieqBy
eqBy :: (a -> b -> Bool) -> DhallList a -> DhallList b -> Bool
eqBy _ Empty Empty = True
eqBy _ Empty _ = False
eqBy _ One{} Empty = False
eqBy f (One x) (One y) = f x y
eqBy _ One{} Mud{} = False
eqBy f (One x) (Vec vy)
  | Vector.length vy == 1 = f x (Vector.unsafeHead vy)
  | otherwise = False
eqBy _ Vec{} Empty = False -- Vec must be non-empty
eqBy f (Vec vx) (One y)
  | Vector.length vx == 1 = f (Vector.unsafeHead vx) y
  | otherwise = False
eqBy f (Vec vx) (Vec vy)
  | Vector.length vx == Vector.length vy = Vector.Generic.eqBy f vx vy
  | otherwise = False
eqBy f (Vec vx) (Mud sy hy ys ly) -- TODO: There's more potential for optimization here
  | Vector.length vx == sy = Vector.Generic.eqBy f vx (mudToVector sy hy ys ly)
  | otherwise = False
eqBy _ Mud{} Empty = False
eqBy _ Mud{} One{} = False
eqBy f (Mud sx hx xs lx) (Vec vy) -- TODO: There's more potential for optimization here
  | sx == Vector.length vy = Vector.Generic.eqBy f (mudToVector sx hx xs lx) vy
  | otherwise = False
eqBy f (Mud sx hx xs lx) (Mud sy hy ys ly) =
      sx == sy
  &&  f hx hy
  &&  f lx ly
  &&  eqByList f (itoList xs) (itoList ys)
{-# inlinable eqBy #-}

eqByList :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqByList _ [] [] = True
eqByList f (x:xs') (y:ys') = f x y && eqByList f xs' ys'
eqByList _ _ _ = False
{-# inlinable eqByList #-}

reverse :: DhallList a -> DhallList a
reverse = \case
  Empty -> Empty
  x@One{} -> x
  Vec v -> case Vector.length v of
    0 -> Empty -- This should never happen, but better be safe
    1 -> singleton (Vector.unsafeHead v)
    n ->
      Mud
        n
        (Vector.unsafeLast v)
        (ireverse (ifromVector (Vector.unsafeInit (Vector.unsafeTail v))))
        (Vector.unsafeHead v)
  Mud s h xs l -> Mud s l (ireverse xs) h
{-# inlinable reverse #-}

length :: DhallList a -> Int
length = \case
  Empty -> 0
  One _ -> 1
  Vec v -> Vector.length v
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
  Vec v -> Just (Vector.unsafeHead v)
  Mud _ x _ _ -> Just x
{-# inlinable head #-}

last :: DhallList a -> Maybe a
last = \case
  Empty -> Nothing
  One x -> Just x
  Vec v -> Just (Vector.unsafeLast v)
  Mud _ _ _ x -> Just x
{-# inlinable last #-}

uncons :: DhallList a -> Maybe (a, DhallList a)
uncons = \case
  Empty -> Nothing
  One x -> Just (x, Empty)
  Vec v -> Just (Vector.unsafeHead v, fromVector (Vector.unsafeTail v))
  x@(Mud _ h _ _) -> Just (h, Vec (Vector.unsafeTail (toVector x)))
{-# inlinable uncons #-}

foldMap :: Monoid m => (a -> m) -> DhallList a -> m
foldMap f = \case
  Empty -> mempty
  One a -> f a
  Vec v -> Data.Foldable.foldMap f v
  Mud _ a xs b -> f a <> ifoldMap f xs <> f b
{-# inlinable foldMap #-}

foldr :: (a -> b -> b) -> b -> DhallList a -> b
foldr f y = \case
  Empty -> y
  One x -> f x y
  Vec v -> Vector.foldr f y v
  Mud _ h xs l -> f h $ ifoldr f (f l y) xs
{-# inlinable foldr #-}

foldr' :: (a -> b -> b) -> b -> DhallList a -> b
foldr' f !y = \case
  Empty -> y
  One x -> f x y
  Vec v -> Vector.foldr' f y v
  Mud _ h xs l -> f h $! ifoldr' f (f l y) xs
{-# inlinable foldr' #-}

foldl' :: (b -> a -> b) -> b -> DhallList a -> b
foldl' f !y = \case
  Empty -> y
  One x -> f y x
  Vec v -> Vector.foldl' f y v
  Mud _ h xs l -> flip f l $! ifoldl' f (f y h) xs
{-# inlinable foldl' #-}

-- | The result is normalized!
map :: (a -> b) -> DhallList a -> DhallList b
map f = \case
  Empty -> Empty
  One x -> One (f x)
  Vec v -> Vec (Vector.map f v)
  x@Mud{} -> Vec (Vector.map f (toVector x))
{-# inlinable map #-}

-- | The result is normalized!
mapWithIndex :: (Int -> a -> b) -> DhallList a -> DhallList b
mapWithIndex f = \case
  Empty -> empty
  One x -> singleton (f 0 x)
  Vec v -> Vec (Vector.imap f v)
  x@Mud{} -> Vec (Vector.imap f (toVector x))
{-# inlinable mapWithIndex #-}

-- TODO: Specialize for Either
mapM_withIndex :: Monad m => (Int -> a -> m ()) -> DhallList a -> m ()
mapM_withIndex f = \case
  Empty -> pure ()
  One x -> f 0 x
  Vec v -> Vector.imapM_ f v
  x@Mud{} -> Vector.imapM_ f (toVector x) -- TODO: Avoid the allocation?
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
  Vec v -> Vec <$> Vector.mapM f v
  x@Mud{} -> Vec <$> Vector.mapM f (toVector x)
{-# inlinable mapM #-}

toList :: DhallList a -> [a]
toList = \case
  Empty -> []
  One a -> [a]
  Vec v -> Vector.toList v
  Mud s h xs l -> case s of
    2 -> [h, l]
    _ -> h : DList.apply (itoDList xs) [l]
{-# inlinable toList #-}

toVector :: DhallList a -> Vector a
toVector = \case
  Empty -> Vector.empty
  One a -> Vector.singleton a
  Vec v -> v
  Mud s h xs l -> mudToVector s h xs l
{-# inlinable toVector #-}

mudToVector :: Int -> a -> Inner a -> a -> Vector a
mudToVector n h xs l
  | n == 2 = Vector.fromListN 2 [h, l]
  | otherwise = Vector.create $ do
      v <- Vector.Mutable.new n -- TODO: Maybe consider using unsafeNew?!
      Vector.Mutable.unsafeWrite v 0 h
      !ix <- iwrite v 1 xs
      -- assert (ix == n - 1)
      Vector.Mutable.unsafeWrite v ix l
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
-- icons x (IRev y) = IRev (ISnoc y x)
icons x y = ICons x y
{-# inlinable icons #-}

isnoc :: Inner a -> a -> Inner a
isnoc IEmpty y = ICons y IEmpty -- Prefer ICons! Why though?
-- isnoc (IRev x) y = IRev (ICons y x)
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
  | Vector.null xs = ifromVector ys
  | Vector.null ys = ifromVector xs
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
  | Vector.null v = IEmpty
  | otherwise = IVec v
{-# inlinable ifromVector #-}

-- TODO: Consider using itoDList / ifoldr?!
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

ifoldr :: (a -> b -> b) -> b -> Inner a -> b
ifoldr f y = \case
  IEmpty -> y
  IVec v -> Vector.foldr f y v
  xs -> DList.foldr f y (itoDList xs)
{-# inlinable ifoldr #-}

-- TODO: Consider using itoDList / ifoldr?!
ifoldr' :: (a -> b -> b) -> b -> Inner a -> b
ifoldr' f !y = \case
  IEmpty -> y
  ICons x xs -> f x $! ifoldr' f y xs
  ISnoc xs x -> ifoldr' f (f x y) xs
  IVec v -> Vector.foldr' f y v
  IRev xs -> ifoldl' (flip f) y xs
  ICat xs0 xs1 -> ifoldr' f (ifoldr' f y xs1) xs0
{-# inlinable ifoldr' #-}

-- TODO: Consider using itoDList / ifoldr?!
ifoldl' :: (b -> a -> b) -> b -> Inner a -> b
ifoldl' f !y = \case
  IEmpty -> y
  ICons x xs -> ifoldl' f (f y x) xs
  ISnoc xs x -> flip f x $! ifoldl' f y xs
  IVec v -> Vector.foldl' f y v
  IRev xs -> ifoldr' (flip f) y xs
  ICat xs0 xs1 -> ifoldl' f (ifoldl' f y xs0) xs1
{-# inlinable ifoldl' #-}

itoList :: Inner a -> [a]
itoList = \case
  IEmpty -> []
  IVec v -> Vector.toList v
  xs -> DList.toList (itoDList xs)
{-# inlinable itoList #-}

-- TODO: Check that this is optimal
itoDList :: Inner a -> DList a
itoDList = \case
  IEmpty -> DList.empty
  ICons x xs -> DList.cons x (itoDList xs)
  ISnoc xs x -> DList.snoc (itoDList xs) x
  IVec v -> Vector.foldr DList.cons DList.empty v
  IRev xs -> case xs of
    IEmpty -> DList.empty
    ICons x xs' -> DList.snoc (itoDList (IRev xs')) x
    ISnoc xs' x -> DList.cons x (itoDList (IRev xs'))
    IVec v -> Vector.foldr (flip DList.snoc) DList.empty v
    IRev ys -> itoDList ys
    ICat xs0 xs1 -> itoDList (IRev xs1) <> itoDList (IRev xs0)
  ICat xs ys -> itoDList xs <> itoDList ys
{-# inlinable itoDList #-}

-- TODO: Prevent reboxing the Int somehow!?
iwrite :: MVector s a -> Int -> Inner a -> ST s Int
iwrite !mv !ix = \case
  IEmpty -> pure ix
  ICons x ys -> do
    Vector.Mutable.unsafeWrite mv ix x
    iwrite mv (ix + 1) ys
  ISnoc xs y -> do
    !ix' <- iwrite mv ix xs
    Vector.Mutable.unsafeWrite mv ix' y
    pure $! ix' + 1
  IVec v -> do
    let !n = Vector.length v
    let slice = Vector.Mutable.unsafeSlice ix n mv
    Vector.unsafeCopy slice v
    pure $! ix + n
  IRev xs0 -> case xs0 of
    IEmpty -> pure ix
    ICons x ys -> do
      !ix' <- iwrite mv ix (IRev ys)
      Vector.Mutable.unsafeWrite mv ix' x
      pure $! ix' + 1
    ISnoc xs y -> do
      Vector.Mutable.unsafeWrite mv ix y
      iwrite mv (ix + 1) (IRev xs)
    IVec v -> do
      let !n = Vector.length v
      let slice = Vector.Mutable.unsafeSlice ix n mv
      Vector.unsafeCopy slice (Vector.reverse v)
      pure $! ix + n
    IRev xs -> iwrite mv ix xs
    ICat xs ys -> do
      !ix' <- iwrite mv ix (IRev ys)
      iwrite mv ix' (IRev xs)
  ICat xs ys -> do
    !ix' <- iwrite mv ix xs
    iwrite mv ix' ys
{-# inlinable iwrite #-}
