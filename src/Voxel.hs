{-# LANGUAGE DeriveFunctor #-}
module Voxel where
import BurningPrelude
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Util
import Data.Witherable
import Control.DeepSeq
import Data.Bits
import Data.Bits.Lens
import Data.Word
import Data.Maybe (isNothing)
import Debug.Trace
import Data.Profunctor.Unsafe

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear deriving (Show, Eq, Ord, Enum)

instance NFData Surface where
  rnf _ = ()

allSurfaces :: [Surface]
allSurfaces = [STop, SBottom, SLeft, SRight, SFront, SRear]

newtype Surfaces = Surfaces { surfaceBits :: Word8 } deriving (Show, Eq, Ord)

fromSurface :: Num a => Surface -> V3 a
fromSurface STop = V3 0 1 0
fromSurface SBottom = V3 0 (-1) 0
fromSurface SLeft = V3 (-1) 0 0
fromSurface SRight = V3 1 0 0
fromSurface SFront = V3 0 0 1
fromSurface SRear = V3 0 0 (-1)
{-# INLINE fromSurface #-}

neumann :: [V3 Int]
neumann = [V3 0 0 1, V3 0 1 0, V3 1 0 0, V3 0 0 (-1), V3 0 (-1) 0, V3 (-1) 0 0]

type instance Index (VoxelWorld a) = V3 Int
type instance IxValue (VoxelWorld a) = a

newtype VoxelWorld a = VoxelWorld { getVoxelWorld :: Map.HashMap (V3 Int) (a, Surfaces) }

makePrisms ''VoxelWorld

instance Ixed (VoxelWorld a) where
  ix v f (VoxelWorld m) = fmap VoxelWorld (ix v (_1 f) m)

instance At (VoxelWorld a) where
  at v f (VoxelWorld m) = f (m ^? ix v . _1) <&> \b -> m
    & at v .~ fmap (,Surfaces ss) b
    & fill v (isNothing b)
    & VoxelWorld
    where
      ss = foldr (\s -> bitAt (fromEnum s) .~ hasn't (ix (v + fromSurface s)) m) 0 allSurfaces

fill v b m = foldr (\s -> ix (v - fromSurface s) . _2 . contains s .~ b) m allSurfaces

type instance Index Surfaces = Surface

instance Contains Surfaces where
  contains s f = fmap Surfaces . bitAt (fromEnum s) f .# surfaceBits
  {-# INLINE contains #-}

data Cube a = Cube !a !a !a !a !a !a deriving Functor

instance Applicative Cube where
  pure a = Cube a a a a a a
  {-# INLINE pure #-}
  Cube f0 f1 f2 f3 f4 f5 <*> Cube x0 x1 x2 x3 x4 x5 = Cube (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)
  {-# INLINE (<*>) #-}

foldSurfaces :: Monoid r => Cube r -> Surfaces -> r
foldSurfaces (Cube a b c d e f) (Surfaces s) = let
  !a0 = if s .&. bit 0 /= 0 then a else mempty
  !a1 = if s .&. bit 1 /= 0 then b <> a0 else a0
  !a2 = if s .&. bit 2 /= 0 then c <> a1 else a1
  !a3 = if s .&. bit 3 /= 0 then d <> a2 else a2
  !a4 = if s .&. bit 4 /= 0 then e <> a3 else a3
  in if s .&. bit 5 /= 0 then f <> a4 else a4
{-# INLINE foldSurfaces #-}

unfoldSurfaces :: Surfaces -> [Surface]
unfoldSurfaces w = BurningPrelude.filter (\s -> w ^. contains s) allSurfaces

surfaces :: V3 Int -> VoxelWorld a -> Surfaces
surfaces v (VoxelWorld m) = maybe (Surfaces 0) id (m ^? ix v . _2)

emptyWorld :: VoxelWorld a
emptyWorld = VoxelWorld Map.empty
