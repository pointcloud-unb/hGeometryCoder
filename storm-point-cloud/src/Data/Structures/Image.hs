module Data.Structures.Image where

import Data.List
import Data.Matrix

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
data ImageSparse = ImageSparse { sPixels :: [Pixel]
                                    , sSide :: Int}
  deriving (Show)

instance Eq ImageSparse where
  (ImageSparse pixel_list_1 _) ==
    (ImageSparse pixel_list_2 _) = pixel_list_1 == pixel_list_2

addPixelToSparse :: ImageSparse -> Pixel -> ImageSparse
addPixelToSparse (ImageSparse list a) pixel = ImageSparse (list ++ [pixel]) a

removePixelToSparse :: ImageSparse -> Pixel -> ImageSparse
removePixelToSparse (ImageSparse list a) pixel = ImageSparse (delete pixel list) a

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
rasterToSparse m = ImageSparse (buildPixelList 1 $ toLists m) (nrows m)

buildPixelList :: Mline -> [[Presence]] -> [Pixel]
buildPixelList y [] = []
buildPixelList y (l:ls)= fillByLine l 1 ++ buildPixelList (y + 1) ls
  where 
    fillByLine [] _ = []
    fillByLine (e:es) x = if e then Pixel y x : fillByLine es (x + 1) else fillByLine es (x + 1)



