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
removePixelToRaster m j i = setElem False (j, i) m

addPixelToRaster :: ImageRaster -> Coordinate -> Coordinate -> ImageRaster
addPixelToRaster m j i = setElem True (j, i) m

-- Transforming images

sparseToRaster :: ImageSparse -> ImageRaster
sparseToRaster (ImageSparse ps s) = foldl check (emptyMatrix s) ps
  where check m (Pixel j i) = addPixelToRaster m j i

rasterToSparse :: ImageRaster -> ImageSparse
rasterToSparse m = ImageSparse (S.fromList (buildPixelList 1 $ toLists m)) (nrows m)

buildPixelList :: MLine -> [[Presence]] -> [Pixel]
buildPixelList i [] = []
buildPixelList i (l:ls)= fillByLine l 1 ++ buildPixelList (i + 1) ls
  where 
    fillByLine [] _ = []
    fillByLine (e:es) j = if e then Pixel i j : fillByLine es (j + 1) else fillByLine es (j + 1)

presenceList2Sparse :: Side -> [Presence] -> ImageSparse
presenceList2Sparse s b = rasterToSparse $ fromList s s b

presenceList2Raster :: Side -> [Presence] -> ImageRaster
presenceList2Raster s = fromList s s