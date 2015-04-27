module Render where
import BurningPrelude
import Assets
import Call
import Voxel
import qualified Data.Vector.Storable as VS

surfaceBitmap :: Bitmap -> [V2 Float] -> Surface -> Scene
surfaceBitmap bmp uvs = \case
  SRear -> mkStrip bmp uvs [V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5)]
  SLeft -> mkStrip bmp uvs [V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 (-0.5), V3 (-0.5) 0.5 0.5]
  SRight -> mkStrip bmp uvs [V3 0.5 (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 0.5, V3 0.5 0.5 (-0.5)]
  STop -> mkStrip bmp uvs [V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5]
  SFront -> mkStrip bmp uvs [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5]
  SBottom -> mkStrip bmp uvs [V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5]
{-# INLINE surfaceBitmap #-}

withSurfaces :: (Cube s -> Rendering s) -> Rendering s
withSurfaces f = withVertices TriangleStrip sRear $ \sre ->
                withVertices TriangleStrip sLeft $ \sle ->
                withVertices TriangleStrip sRight $ \sri ->
                withVertices TriangleStrip sTop $ \sto ->
                withVertices TriangleStrip sFront $ \sfr ->
                withVertices TriangleStrip sBottom $ \sbo -> f $ Cube sto sbo sle sri sfr sre

uvSquare :: [V2 Float]
uvSquare = [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

sRear, sLeft, sRight, sTop, sFront, sBottom :: VS.Vector Vertex
sRear = VS.fromList $! zipWith positionUV [V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5)] uvSquare
sLeft = VS.fromList $! zipWith positionUV [V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 (-0.5), V3 (-0.5) 0.5 0.5] uvSquare
sRight = VS.fromList $! zipWith positionUV [V3 0.5 (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 0.5, V3 0.5 0.5 (-0.5)] uvSquare
sTop = VS.fromList $! zipWith positionUV [V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5] uvSquare
sFront = VS.fromList $! zipWith positionUV [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5] uvSquare
sBottom = VS.fromList $! zipWith positionUV [V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5] uvSquare

mkStrip bmp uvs vs = Scene $ vertices bmp TriangleStrip $ VS.fromList $ zipWith positionUV vs uvs
{-# INLINE mkStrip #-}

skybox :: Scene
skybox = scale (V3 (-128) 128 128)
   $ surfaceBitmap _skybox_png [V2 (1/3) 1, V2 (2/3) 1, V2 (1/3) 0.5, V2 (2/3) 0.5] SRear
  <> surfaceBitmap _skybox_png [V2 (1/3) 1, V2 0 1, V2 (1/3) 0.5, V2 0 0.5] SLeft
  <> surfaceBitmap _skybox_png [V2 (2/3) 1, V2 1 1, V2 (2/3) 0.5, V2 1 0.5] SRight
  <> surfaceBitmap _skybox_png [V2 (1/3) 0.5, V2 (2/3) 0.5, V2 (1/3) 0, V2 (2/3) 0] STop
  <> surfaceBitmap _skybox_png [V2 1 0.5, V2 (2/3) 0.5, V2 1 0, V2 (2/3) 0] SFront
  <> surfaceBitmap _skybox_png [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5] SBottom
