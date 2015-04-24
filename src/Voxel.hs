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

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear deriving (Show, Eq, Ord, Enum)

instance NFData Surface where
  rnf _ = ()

allSurfaces :: [Surface]
allSurfaces = [STop, SBottom, SLeft, SRight, SFront, SRear]

newtype Surfaces = Surfaces Word8 deriving (Show, Eq, Ord)

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

newtype VoxelWorld a = VoxelWorld (Map.HashMap (V3 Int) (a, Surfaces))

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
  contains s f (Surfaces w) = fmap Surfaces $ bitAt (fromEnum s) f w

unfoldSurfaces :: Surfaces -> [Surface]
unfoldSurfaces w = BurningPrelude.filter (\s -> w ^. contains s) allSurfaces

surfaces :: V3 Int -> VoxelWorld a -> [Surface]
surfaces v (VoxelWorld m) = m ^.. ix v . _2 . to unfoldSurfaces . folded

emptyWorld :: VoxelWorld a
emptyWorld = VoxelWorld Map.empty
