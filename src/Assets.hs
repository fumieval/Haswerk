module Assets where
import BurningPrelude
import Data.Monoid
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Control.Lens
import Data.ByteString.Lens
import System.IO.Unsafe
import System.FilePath
import System.FilePath.Lens
import Paths_Haswerk
import System.IO.Unsafe
import Codec.Picture

_skybox_png = unsafePerformIO $ readImageRGBA8 "assets/skybox.png"

_terrain_png = unsafePerformIO $ readImageRGBA8 "assets/terrain.png"

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img
fromDynamicImage _ = error "Unsupported format"

readImageRGBA8 :: FilePath -> IO (Image PixelRGBA8)
readImageRGBA8 path = readImage path >>= either fail (return . fromDynamicImage)

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id