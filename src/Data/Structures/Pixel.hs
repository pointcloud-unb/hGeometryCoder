module Data.Structures.Pixel where
import Data.Utils

-- Pixel type
data Pixel = Pixel Coordinate Coordinate
    deriving(Show)

instance Eq Pixel where
    -- (==) Pixel -> Pixel -> Bool
    (Pixel u1 v1) == (Pixel u2 v2) = (u1 == u2) && (v1 == v2)

    -- (!=) Pixel -> Pixel -> Bool
    (Pixel u1 v1) /= (Pixel u2 v2) = (u1 /= u2) || (v1 /= v2)

instance Ord Pixel where
  -- Less comparison
  (Pixel u1 v1) < (Pixel u2 v2) 
    = if u1 == u2 then
         v1 < v2
      else
         u1 < u2
  -- Less or Equal comparison
  (Pixel u1 v1) <= (Pixel u2 v2) 
    = if u1 == u2 then
        v1 <= v2
      else
        u1 <= u2
  -- Greater comparison
  (Pixel u1 v1) > (Pixel u2 v2) 
    = if u1 == u2 then
        v1 > v2 
      else
        u1 > u2
  -- Greater or Equal comparison
  (Pixel u1 v1) >= (Pixel u2 v2) 
    = if u1 == u2 then
        v1 >= v2
      else
        u1 >= u2