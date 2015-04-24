module World where
import BurningPrelude
import Voxel
import Control.Lens
import Linear
import Block
import qualified Data.Heap as Heap
import Data.Complex
import Data.Array.IO
import System.Random

data World = World
  { _blocks :: !(VoxelWorld Block)
  , _blockUpdate :: !(Heap.Heap (Heap.Entry Float (V3 Int)))
  }
makeLenses ''World


newWorld :: IO World
newWorld = do
  {-
  ar <- newArray ((-50, -50), (50, 50)) 0 :: IO (IOArray (Int, Int) Int)
  replicateM_ 1000 $ do
    x <- randomRIO (-1 :: Float, 1)
    y <- randomRIO (-1 :: Float, 1)
    case buddha (x :+ y) (0 :+ 0) 300 of
      Just xs -> forM_ xs $ \(a :+ b) -> do
        let i = round (a * 20)
        let j = round (b * 20)
        v <- readArray ar (i, j)
        writeArray ar (i, j) (v + 1)
      Nothing -> return ()
  s <- getAssocs ar
  -}
  let s = [((i, j), 0) | i <- [0..64], j <- [0..64]]
  return $ flip execState (World emptyWorld Heap.empty)
    $ for_ [(V3 i y j, dirt) | ((i, j), l) <- s, y <- [0..l]]
    $ \(i, b) -> placeBlock i b
buddha c z i
  | i == 0 = Nothing
  | magnitude z > 2 = Just []
  | otherwise = fmap (z:) (buddha c (z * z + c) (i - 1))

causeBlockUpdate :: (MonadState World m) => V3 Int -> m ()
causeBlockUpdate v = blockUpdate %= Heap.insert (Heap.Entry 0 v)

placeBlock :: (MonadState World m) => V3 Int -> Block -> m ()
placeBlock v b = do
  blocks . at v ?= b
  causeBlockUpdate v
