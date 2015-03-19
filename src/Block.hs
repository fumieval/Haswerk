module Block where
import Call
import Render
import Voxel
import Assets
import BurningPrelude

type Block = Mortal Action IO ()

data Action x where
  Render :: Time -> Action (Surface -> Scene)
  Damage :: Float -> Action ()

dirt = solid _dirt_png 2 2
stoneBrick = solid _stonebrick_png 5 5

solid :: Bitmap -> Float -> Float -> Block
solid bmp !mf !f = mortal $ \case
  Render dt -> if f < 0
    then left ()
    else return (color (V4 c c c 1)
      . surfaceBitmap bmp [V2 0 0, V2 1 0, V2 0 1, V2 1 1], solid bmp mf (min mf (f + dt / 60)))
  Damage d -> return ((), solid bmp mf (f - d))
  where
    c = f / mf
