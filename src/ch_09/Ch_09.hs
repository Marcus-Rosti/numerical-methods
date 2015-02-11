module Ch_09
    (boundedMin
    ) where

boundedMin :: (Double -> Double) -- ^ function
 -> Double -- ^ leftmost point
 -> Double -- ^ rightmost point
 -> Double -- ^ error bound
 -> Double -- ^ result within bound
boundedMin func a b err
  | abs (x3 - x0) <= err = (x0 + x3) / 2
  | func x1 > func x2 = boundedMin func x1 x3 err
  | otherwise = boundedMin func x0 x2 err
    where
      phi = (sqrt 5 - 1) / 2
      x0 = a
      x1 = a + (b-a) * (1-phi)
      x2 = a + (b-a) * phi
      x3 = b
