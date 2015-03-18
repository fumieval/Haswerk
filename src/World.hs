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
  [(V3 c 0 r, dirt) | c <- [0..0], r <- [0..0]]
