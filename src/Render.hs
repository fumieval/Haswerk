module Render where
import BurningPrelude
import Assets
import Call
import Voxel
import qualified Data.Vector.Storable as VS

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

skybox :: Scene
skybox = scale (V3 128 128 128)
   $ surfaceBitmap _skybox_png [V2 (1/3) 1, V2 (2/3) 1, V2 (1/3) 0.5, V2 (2/3) 0.5] SRear
  <> surfaceBitmap _skybox_png [V2 (1/3) 1, V2 0 1, V2 (1/3) 0.5, V2 0 0.5] SLeft
  <> surfaceBitmap _skybox_png [V2 (2/3) 1, V2 1 1, V2 (2/3) 0.5, V2 1 0.5] SRight
  <> surfaceBitmap _skybox_png [V2 (1/3) 0.5, V2 (2/3) 0.5, V2 (1/3) 0, V2 (2/3) 0] STop
  <> surfaceBitmap _skybox_png [V2 1 0.5, V2 (2/3) 0.5, V2 1 0, V2 (2/3) 0] SFront
  <> surfaceBitmap _skybox_png [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5] SBottom
