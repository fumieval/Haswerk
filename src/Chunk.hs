module Chunk where

import qualified Data.Vector.Storable as V

type Chunks = HM.HashMap (V3 Int) VertexBuffer

emptyChunks :: Map.HashMap (V3 Int) VertexBuffer
emptyChunks = Map.empty

drawChunks :: Given System => M44 Float -> Texture -> Chunks -> IO ()
drawChunks v tex = itraverse (\i buf -> drawVertex (tr !*! v) tex buf) where
  tr = identity & translation .~ fmap fromIntegral i * chunkSize

chunkSize :: Int
chunkSize = 8

renderChunk :: (V3 Int -> IO [Vertex]) -> StateT s IO VertexBuffer
renderChunk f = do
  vs <- fmap concat $ traverse f ()
  registerVertex Triangles (V.fromList vs)
