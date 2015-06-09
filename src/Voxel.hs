{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Voxel where
import BurningPrelude
import Data.Distributive
import qualified Data.HashMap.Strict as HM

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear

allSurfaces :: [Surface]
allSurfaces = [STop, SBottom, SLeft, SRight, SFront, SRear]

fromSurface :: Num a => Surface -> V3 a
fromSurface STop = V3 0 1 0
fromSurface SBottom = V3 0 (-1) 0
fromSurface SLeft = V3 (-1) 0 0
fromSurface SRight = V3 1 0 0
fromSurface SFront = V3 0 0 1
fromSurface SRear = V3 0 0 (-1)
{-# INLINE fromSurface #-}

data Cube a = Cube !a !a !a !a !a !a deriving (Functor, Foldable, Traversable)

instance Monoid a => Monoid (Cube a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Distributive Cube where
  distribute = distributeRep

instance Representable Cube where
  type Rep Cube = Surface
  tabulate f = Cube (f STop) (f SBottom) (f SLeft) (f SRight) (f SFront) (f SRear)
  index (Cube a b c d e f) = \case
    STop -> a
    SBottom -> b
    SLeft -> c
    SRight -> d
    SFront -> e
    SRear -> f

instance Applicative Cube where
  pure a = Cube a a a a a a
  {-# INLINE pure #-}
  Cube f0 f1 f2 f3 f4 f5 <*> Cube x0 x1 x2 x3 x4 x5 = Cube (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)
  {-# INLINE (<*>) #-}

foldVoxel :: Monoid r => (V3 Int -> Cube (Maybe a) -> a -> r) -> [(V3 Int, a)] -> r
foldVoxel f xs = foldMap (\(v, a) -> f v (neighbors v) a) xs where
  neighbors v = tabulate $ \s -> HM.fromList xs ^? ix (v + fromSurface s)