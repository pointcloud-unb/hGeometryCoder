module Data.Structures.Image where

import Data.List
import Data.Matrix
import qualified Data.Set as S
import Data.Structures.Pixel
import Data.Utils

-- Image Sparced type
data ImageSparse = ImageSparse { sPixels :: S.Set Pixel
                               , sSide   :: Int }
  deriving (Show)

instance Eq ImageSparse where
  (ImageSparse pixel_set_1 s1) ==
    (ImageSparse pixel_set_2 s2) = pixel_set_1 == pixel_set_2 && s1 == s2

addPixelToSparse :: ImageSparse -> Pixel -> ImageSparse
addPixelToSparse (ImageSparse set side) pixel = ImageSparse (S.insert pixel set) side

removePixelToSparse :: ImageSparse -> Pixel -> ImageSparse
removePixelToSparse (ImageSparse set side) pixel = ImageSparse (S.delete pixel set) side

-- 2D Matrix
type Presence = Bool
type Side = Int
type MLine = Int
type ImageRaster = Matrix Presence

emptyMatrix :: Side -> ImageRaster
emptyMatrix s = matrix s s (\(i, j) -> False)

removePixelToRaster :: ImageRaster -> Coordinate -> Coordinate -> ImageRaster
removePixelToRaster m x y = setElem False (x, y) m

addPixelToRaster :: ImageRaster -> Coordinate -> Coordinate -> ImageRaster
addPixelToRaster m x y = setElem True (x, y) m

-- Transforming images

sparseToRaster :: ImageSparse -> ImageRaster
sparseToRaster (ImageSparse ps s) = foldl check (emptyMatrix s) ps
  where check m (Pixel x y) = addPixelToRaster m x y

rasterToSparse :: ImageRaster -> ImageSparse
rasterToSparse m = ImageSparse (S.fromList (buildPixelList 1 $ toLists m)) (nrows m)

buildPixelList :: MLine -> [[Presence]] -> [Pixel]
buildPixelList y [] = []
buildPixelList y (l:ls)= fillByLine l 1 ++ buildPixelList (y + 1) ls
  where 
    fillByLine [] _ = []
    fillByLine (e:es) x = if e then Pixel y x : fillByLine es (x + 1) else fillByLine es (x + 1)



