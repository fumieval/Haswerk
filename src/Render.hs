{-# LANGUAGE LambdaCase #-}
module Render where
import World
import Assets
import Call
import Voxel
import Data.Monoid
import qualified Data.Vector.Storable as VS
import Control.Lens

crosshair :: Sight
crosshair = viewPicture $ translate (V2 320 240) $ bitmap _crosshair_png

surfaceBitmap :: Bitmap -> [V2 Float] -> Surface -> Scene
surfaceBitmap bmp uvs = \case
  SRear -> mk [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0]
  SLeft -> mk [V3 0 0 0, V3 0 0 1, V3 0 1 0, V3 0 1 1] 
  SRight -> mk [V3 1 0 0, V3 1 0 1, V3 1 1 0, V3 1 1 1]
  STop -> mk [V3 0 1 0, V3 1 1 0, V3 0 1 1, V3 1 1 1]
  SFront -> mk [V3 0 0 1, V3 1 0 1, V3 0 1 1, V3 1 1 1]
  SBottom -> mk [V3 0 0 0, V3 1 0 0, V3 0 0 1, V3 1 0 1]
  where
    mk vs = vertices bmp TriangleStrip $ VS.fromList $ map offset $ zipWith (flip positionUV) uvs vs
    {-# INLINE mk #-}
    offset (Vertex p uv n) = Vertex (p - pure 0.5) uv n
    {-# INLINE offset #-}
{-# INLINE surfaceBitmap #-}

renderBlock :: Block -> [Surface] -> Scene
renderBlock (Block b) ss = case b of
  Dirt -> r _dirt_png
  StoneBrick -> r _stonebrick_png
  where
    r bmp = mconcat $ map (surfaceBitmap bmp [V2 0 0, V2 1 0, V2 0 1, V2 1 1]) ss

renderBlocks :: VoxelWorld Block -> Scene
renderBlocks (VoxelWorld m s) = mconcat [translate (fmap fromIntegral p)
  $ renderBlock b (s ^.. ix p . folded)
  | (p, b) <- m ^@.. itraversed]

skybox :: Scene
skybox = scale (V3 128 128 128)
   $ surfaceBitmap _skybox_png [V2 (1/3) 1, V2 (2/3) 1, V2 (1/3) 0.5, V2 (2/3) 0.5] SRear
  <> surfaceBitmap _skybox_png [V2 (1/3) 1, V2 0 1, V2 (1/3) 0.5, V2 0 0.5] SLeft
  <> surfaceBitmap _skybox_png [V2 (2/3) 1, V2 1 1, V2 (2/3) 0.5, V2 1 0.5] SRight
  <> surfaceBitmap _skybox_png [V2 (1/3) 0.5, V2 (2/3) 0.5, V2 (1/3) 0, V2 (2/3) 0] STop
  <> surfaceBitmap _skybox_png [V2 1 0.5, V2 (2/3) 0.5, V2 1 0, V2 (2/3) 0] SFront
  <> surfaceBitmap _skybox_png [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5] SBottom

renderWorld :: V3 Float -> World -> Scene
renderWorld pos world = mconcat [translate pos skybox, renderBlocks $ _blocks world]