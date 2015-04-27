{-# LANGUAGE DeriveFunctor #-}
module Block where
import Call
import Render
import Voxel
import Assets
import BurningPrelude

type Block = Mortal Action IO ()

data Action x where
  Render :: Time -> Action (Cube Bitmap)
  Damage :: Float -> Action ()

dirt :: Block
dirt = mortal $ \case
  Render dt -> return (Cube _grass_png _dirt_png _dirt_png _dirt_png _dirt_png _dirt_png, dirt)
  Damage d -> left ()

stoneBrick :: Block
stoneBrick = mortal $ \case
  Render dt -> return (Cube _stonebrick_png _stonebrick_png _stonebrick_png _stonebrick_png _stonebrick_png _stonebrick_png, stoneBrick)
  Damage d -> left ()
