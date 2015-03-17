module World where
import Voxel
import Control.Lens
import Linear

data BlockType = StoneBrick | Dirt

data Block = Block
  { _blockType :: BlockType }
makeLenses ''Block

instance Substantial Block where
  isOpaque _ = True

data World = World
  { _blocks :: VoxelWorld Block
  }
makeLenses ''World

newWorld = World genFloor

genFloor = foldr (\(i, v) -> voxelAt i ?~ v)
  emptyWorld
  [(V3 c 0 r, Block Dirt) | c <- [-16..16], r <- [-16..16]]
