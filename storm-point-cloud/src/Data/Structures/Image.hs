module Data.Structures.Image where

import Data.List
import Data.Matrix
import qualified Data.Set as S

-- Pixel type -- MEMORY RELATED
type Coordinate = Int
data Pixel = Pixel Coordinate Coordinate
    deriving(Show)

instance Eq Pixel where
    -- (==) Pixel -> Pixel -> Bool
    (Pixel x1 y1) == (Pixel x2 y2) = (x1 == x2) && (y1 == y2)

    -- (!=) Pixel -> Pixel -> Bool
    (Pixel x1 y1) /= (Pixel x2 y2) = (x1 /= x2) || (y1 /= y2)

instance Ord Pixel where
  -- Less comparison
  (Pixel x1 y1) < (Pixel x2 y2) 
    = if x1 == x2 then
         y1 < y2
      else
         x1 < x2
  -- Less or Equal comparison
  (Pixel x1 y1) <= (Pixel x2 y2) 
    = if x1 == x2 then
        y1 <= y2
      else
        x1 <= x2
  -- Greater comparison
  (Pixel x1 y1) > (Pixel x2 y2) 
    = if x1 == x2 then
        y1 > y2 
      else
        x1 > x2
  -- Greater or Equal comparison
  (Pixel x1 y1) >= (Pixel x2 y2) 
    = if x1 == x2 then
        y1 >= y2
      else
        x1 >= x2

-- Image Sparced type
data ImageSparse = ImageSparse { sPixels :: S.Set Pixel
                               , sSide   :: Int }
  deriving (Show)

instance Eq ImageSparse where
  (ImageSparse pixel_set_1 _) ==
    (ImageSparse pixel_set_2 _) = pixel_set_1 == pixel_set_2

addPixelToSparse :: ImageSparse -> Pixel -> ImageSparse
addPixelToSparse (ImageSparse set a) pixel = ImageSparse (S.insert pixel set) a

removePixelToSparse :: ImageSparse -> Pixel -> ImageSparse
removePixelToSparse (ImageSparse set a) pixel = ImageSparse (S.delete pixel set) a

-- 2D Matrix
type Presence = Bool
type Side = Int
type Mline = Int
-- Always working with square matrixes
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

buildPixelList :: Mline -> [[Presence]] -> [Pixel]
buildPixelList y [] = []
buildPixelList y (l:ls)= fillByLine l 1 ++ buildPixelList (y + 1) ls
  where 
    fillByLine [] _ = []
    fillByLine (e:es) x = if e then Pixel y x : fillByLine es (x + 1) else fillByLine es (x + 1)



