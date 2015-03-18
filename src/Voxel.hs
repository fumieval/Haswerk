module Voxel where
import BurningPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as A
import Util
import Data.Witherable

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear deriving (Show, Eq, Ord)

allSurfaces :: [Surface]
allSurfaces = [STop, SBottom, SLeft, SRight, SFront, SRear]

fromSurface :: Num a => Surface -> V3 a
fromSurface STop = V3 0 1 0
fromSurface SBottom = V3 0 (-1) 0
fromSurface SLeft = V3 (-1) 0 0
fromSurface SRight = V3 1 0 0
fromSurface SFront = V3 0 0 1
fromSurface SRear = V3 0 0 (-1)

neumann :: [V3 Int]
neumann = [V3 0 0 1, V3 0 1 0, V3 1 0 0, V3 0 0 (-1), V3 0 (-1) 0, V3 (-1) 0 0]

type ChunkMap a = Map.Map (V3 Int) (A.Array (V3 Int) a)

newtype VoxelWorld a = VoxelWorld (ChunkMap (Maybe a))

chunkSize :: Int
chunkSize = 16

chunkIx = (pure 0, pure (chunkSize - 1))

voxelAt :: V3 Int -> Lens' (VoxelWorld a) (Maybe a)
voxelAt v f (VoxelWorld m) = f (m ^? ix ch . ix i . _Just) <&> \b -> case m ^? ix ch of
  Just c -> m & ix ch .~ (c & ix i .~ b) & VoxelWorld
  Nothing -> m
    & at ch ?~ A.accumArray (const id) Nothing chunkIx [(i, b)] -- insert
    & VoxelWorld
  where
    (ch, i) = toChunkIndex v

toChunkIndex = liftA2 (,) (fmap (`div` chunkSize)) (fmap (`mod` chunkSize))

voxelIx :: V3 Int -> Traversal' (VoxelWorld a) a
voxelIx v f (VoxelWorld m) = fmap VoxelWorld $ (ix ch . ix i . _Just $ f) m where
  (ch, i) = toChunkIndex v

emptyWorld :: VoxelWorld a
emptyWorld = VoxelWorld Map.empty

instance WitherableWithIndex (V3 Int, [Surface]) VoxelWorld where
  iwither f (VoxelWorld m) = fmap VoxelWorld
    $ ifor m $ \ch a -> fmap (A.listArray chunkIx)
    $ for (A.assocs a) $ \(i, e) -> do
      let ss = filter (\s -> hasn't (ix (i + fromSurface s) . _Just) a) allSurfaces
      wither (f (ch * 16 + i, ss)) e

