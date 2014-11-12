{-# LANGUAGE LambdaCase, Rank2Types #-}
module Voxel where
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Linear

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

class Substantial a where
  isOpaque :: a -> Bool

data VoxelWorld a = VoxelWorld !(Map.Map (V3 Int) a) !(Map.Map (V3 Int) (Set.Set Surface))

prepare :: V3 Int -> a -> [V3 Int] -> Map.Map (V3 Int) a -> Map.Map (V3 Int) a
prepare v a = flip $ foldr (\d -> at (v + d) %~ maybe (Just a) Just)

voxelAt :: Substantial a => V3 Int -> Lens' (VoxelWorld a) (Maybe a)
voxelAt v f (VoxelWorld m s) = f (Map.lookup v m) <&> \case
  Nothing -> VoxelWorld (Map.delete v m) $ s
    & Map.delete v
    & flip (foldr (\d -> ix (v - fromSurface d) . contains d .~ True)) allSurfaces
  Just a -> VoxelWorld (Map.insert v a m) $ s
    & prepare v Set.empty neumann
    & at v ?~ Set.fromList allSurfaces Set.\\ Set.fromList [s | s <- allSurfaces, b <- m ^.. ix (v + fromSurface s), isOpaque b]
    & if isOpaque a then flip (foldr (\d -> ix (v - fromSurface d) . contains d .~ False)) allSurfaces else id

emptyWorld = VoxelWorld Map.empty Map.empty