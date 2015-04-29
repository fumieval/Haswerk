module Render where
import BurningPrelude
import Assets
import Call
import Voxel
import qualified Data.Vector.Storable as VS

withSurfaces :: (Cube s -> Rendering s) -> Rendering s
withSurfaces f = withVertices Triangles sRear $ \sre ->
                withVertices Triangles sLeft $ \sle ->
                withVertices Triangles sRight $ \sri ->
                withVertices Triangles sTop $ \sto ->
                withVertices Triangles sFront $ \sfr ->
                withVertices Triangles sBottom $ \sbo -> f $ Cube sto sbo sle sri sfr sre

uvSquare :: [V2 Float]
uvSquare = [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

sRear, sLeft, sRight, sTop, sFront, sBottom :: VS.Vector Vertex
sRear = genStrip $ zipWith positionUV [V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5)] uvSquare
sLeft = genStrip $ zipWith positionUV [V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 (-0.5), V3 (-0.5) 0.5 0.5] uvSquare
sRight = genStrip $ zipWith positionUV [V3 0.5 (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 0.5, V3 0.5 0.5 (-0.5)] uvSquare
sTop = genStrip $ zipWith positionUV [V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5] uvSquare
sFront = genStrip $ zipWith positionUV [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5] uvSquare
sBottom = genStrip $ zipWith positionUV [V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5] uvSquare

genStrip (x : y : zs) = VS.fromList $ go x y zs where
  go a b (c : cs) = a : b : c : go c b cs
  go _ _ [] = []

renderStrip bmp uvs vs = Scene $ vertices bmp TriangleStrip $ VS.fromList $ zipWith positionUV vs uvs
{-# INLINE renderStrip #-}

skybox :: Scene
skybox = scale (V3 (-256) 256 256)
   $ renderStrip _skybox_png [V2 (2/3) 1, V2 (1/3) 1, V2 (2/3) 0.5, V2 (1/3) 0.5] [V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5)]
  <> renderStrip _skybox_png [V2 (1/3) 1, V2 0 1, V2 (1/3) 0.5, V2 0 0.5] [V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 (-0.5), V3 (-0.5) 0.5 0.5]
  <> renderStrip _skybox_png [V2 1 1, V2 (2/3) 1, V2 1 0.5, V2 (2/3) 0.5] [V3 0.5 (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 0.5, V3 0.5 0.5 (-0.5)]
  <> renderStrip _skybox_png [V2 (2/3) 0.5, V2 (1/3) 0.5, V2 (2/3) 0, V2 (1/3) 0] [V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5]
  <> renderStrip _skybox_png [V2 1 0.5, V2 (2/3) 0.5, V2 1 0, V2 (2/3) 0] [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5]
  <> renderStrip _skybox_png [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5] [V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5]
