module Voxel where
import BurningPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Util
import Data.Witherable
import Control.DeepSeq
import Data.Bits

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear deriving (Show, Eq, Ord)

instance NFData Surface where
  rnf _ = ()

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

neumann :: [V3 Int]
neumann = [V3 0 0 1, V3 0 1 0, V3 1 0 0, V3 0 0 (-1), V3 0 (-1) 0, V3 (-1) 0 0]

type ChunkMap a = Map.Map (V3 Int) (V.Vector a)

newtype VoxelWorld a = VoxelWorld (ChunkMap (Maybe a))

chunkSize :: Int
chunkSize = 8

voxelAt :: V3 Int -> Lens' (VoxelWorld a) (Maybe a)
voxelAt v f (VoxelWorld m) = f (m ^? ix ch . ix i . _Just) <&> \b -> case m ^? ix ch of
  Just c -> m & ix ch .~ (c & ix i .~ b) & VoxelWorld
  Nothing -> m
    & at ch ?~ V.generate (chunkSize*chunkSize*chunkSize) (\j -> if i == j then b else Nothing)
    & VoxelWorld
  where
    (ch, i) = toChunkIndex v

chunkIndex :: Iso' (V3 Int) Int
chunkIndex = iso go back where
  go (V3 a b c) = ((a * chunkSize) + b) * chunkSize + c
  {-# INLINE go #-}
  back n = let (m, c) = divMod n chunkSize
               (a, b) = divMod m chunkSize
            in V3 a b c
  {-# INLINE back #-}
{-# INLINE chunkIndex #-}

toChunkIndex :: V3 Int -> (V3 Int, Int)
toChunkIndex v = (fmap (`div` chunkSize) v, fmap (`mod` chunkSize) v ^. chunkIndex)
{-# INLINE toChunkIndex #-}

voxelIx :: V3 Int -> Traversal' (VoxelWorld a) a
voxelIx v f (VoxelWorld m) = fmap VoxelWorld $ (ix ch . ix i . _Just $ f) m where
  (ch, i) = toChunkIndex v
{-# INLINE voxelIx #-}

emptyWorld :: VoxelWorld a
emptyWorld = VoxelWorld Map.empty

instance WitherableWithIndex (V3 Int, [Surface]) VoxelWorld where
  iwither f (VoxelWorld m) = fmap VoxelWorld
    $ ifor m $ \ch !vec -> ifor vec $ \i me -> case me of
      Nothing -> pure Nothing
      Just e -> let !p = fmap (`shiftL`3) ch + review chunkIndex i
                    ss = filter (\s -> hasn't (ix (i + fromSurface s ^. chunkIndex) . _Just) vec) allSurfaces
                in f (p, ss) e
