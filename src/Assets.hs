module Assets where
import BurningPrelude
import Call
import Data.Monoid
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
-- import qualified Data.PMD as PMD
import qualified Data.ByteString.Lazy as BL
import Control.Lens
import Data.ByteString.Lens
import System.IO.Unsafe
import System.FilePath
import System.FilePath.Lens
import Paths_Haswerk
import System.IO.Unsafe

_skybox_png = unsafePerformIO $ readBitmap "assets/skybox.png"

_crosshair_png = unsafePerformIO $ readBitmap "assets/crosshair.png"

_stonebrick_png = unsafePerformIO $ readBitmap "assets/stonebrick.png"
_dirt_png = unsafePerformIO $ readBitmap "assets/dirt.png"

_grass_png = unsafePerformIO $ readBitmap "assets/grass.png"

-- Textures: http://forum.minecraftuser.jp/viewtopic.php?t=14102
-- loadBitmapsWith [|return|] "../assets"
{-
fromPMDVertex :: PMD.Vertex -> Vertex
fromPMDVertex v
  | PMD.ve v == 1 = Vertex (p + n) uv n
  | otherwise = Vertex p uv n where
    p = fmap realToFrac $ V3 (PMD.vpx v) (PMD.vpy v) (PMD.vpz v)
    uv = fmap realToFrac $ V2 (PMD.vu v) (PMD.vv v)
    n = fmap realToFrac $ V3 (PMD.vnx v) (PMD.vny v) (PMD.vnz v)

fromPMDMaterial :: PMD.Material -> Color
fromPMDMaterial m = blend 0.5
  (RGBA (realToFrac $ PMD.mdr m) (realToFrac $ PMD.mdg m) (realToFrac $ PMD.mdb m) (realToFrac $ PMD.mda m))
  (RGBA (realToFrac $ PMD.mar m) (realToFrac $ PMD.mag m) (realToFrac $ PMD.mab m) 1)

fromPMD :: FilePath -> IO Scene
fromPMD path = do
  Right (PMD.PMD _ _ (PMD.VertexVec vv) (PMD.IndexInfo vi) (PMD.Materials ms)) <- decodeLazy <$> BL.readFile path
  let go pos (m:ms) = do
        let n = fromIntegral (PMD.mvs m)
        let path0 = PMD.mfilename m ^. unpackedChars
        let (path',rest) = break (=='*') $ takeWhile (/=toEnum 0) path0
        print $ fromIntegral (PMD.mn m)
        bmp <- if path' == ""
          then return Blank
          else readBitmap $ takeDirectory path </> path'
        f <- if rest == "" then return id
          else do
            b <- readBitmap $ takeDirectory path </> tail rest
            return $ applyVFX . if last rest == 'h' then SphericalAdd b else SphericalMultiply b
        s <- go (pos + n) ms
        return $ mappend s
          $ color (fromPMDMaterial m)
          $ f
          $ vertices bmp Triangles
          $ VS.fromList [fromPMDVertex $ vv V.! fromIntegral (vi V.! i)
          | i <- [pos..pos+n-1]]
      go _ [] = return mempty
  go 0 (V.toList ms)
-}
