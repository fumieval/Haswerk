module Block where
import Call
import Render
import Voxel
import Assets
import BurningPrelude

type Block = Mortal Action IO ()

data Action x where
  Render :: Time -> Action (Surface -> Bitmap)
  Damage :: Float -> Action ()

dirt :: Block
dirt = mortal $ \case
  Render dt -> return (\case
    STop -> _grass_png
    _ -> _dirt_png, dirt)
  Damage d -> left ()

stoneBrick :: Block
stoneBrick = mortal $ \case
  Render dt -> return (const _stonebrick_png, stoneBrick)
  Damage d -> left ()
