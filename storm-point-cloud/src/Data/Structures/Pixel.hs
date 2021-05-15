module Data.Structures.Pixel where
import Data.Utils

-- Pixel type
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