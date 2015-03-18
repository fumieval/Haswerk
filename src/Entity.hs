module Entity where
import BurningPrelude
import Voxel

data Target = TBlock (V3 Int) Surface
  | TNone
