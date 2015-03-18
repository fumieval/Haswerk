module World where
import BurningPrelude
import Voxel
import Control.Lens
import Linear
import Block

data World = World
  { _blocks :: !(VoxelWorld Block)
  }
makeLenses ''World

newWorld = World genFloor

genFloor = foldr (\(i, v) -> voxelAt i ?~ v)
  emptyWorld
  [(V3 c d r, dirt) | c <- [-16..16], r <- [-16..16], d <- [-2..0]]
