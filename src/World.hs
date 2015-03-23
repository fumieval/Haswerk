module World where
import BurningPrelude
import Voxel
import Control.Lens
import Linear
import Block
import qualified Data.Heap as Heap

data World = World
  { _blocks :: !(VoxelWorld Block)
  , _blockUpdate :: !(Heap.Heap (Heap.Entry Float (V3 Int)))
  }
makeLenses ''World

newWorld = flip execState (World emptyWorld Heap.empty)
  $ for_ [(V3 c d r, dirt) | c <- [-16..15], r <- [-16..15], d <- [-15..0]]
  $ \(i, b) -> placeBlock i b

causeBlockUpdate :: (MonadState World m) => V3 Int -> m ()
causeBlockUpdate v = blockUpdate %= Heap.insert (Heap.Entry 0 v)

placeBlock :: (MonadState World m) => V3 Int -> Block -> m ()
placeBlock v b = do
  blocks . at v ?= b
  causeBlockUpdate v
