{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}


module MultiKill.Model (Model (..)) where

import Data.List


-- | MTLR Model data type
data Model = Model { numTimePoints :: Integer -- ^ Number of time points in the interval.
                   , timePoints :: [Double] -- ^ Time points in the interval.
                   , rForSomeReason :: Integer -- ^ I have no idea what is this even?
                   , dimension :: Integer -- ^ Number of features in the model.
                   , features :: [(Integer, Double)]  -- ^ Feature, value pairs
                   }

instance Show Model where
  show model = intercalate "\n" [numTimes, times, r, dim, feats]
    where numTimes = "m:" ++ show (numTimePoints model)
          times = intercalate "," (map show $ timePoints model)
          r = "r:" ++ show (rForSomeReason model)
          dim = "DIM:" ++ show (dimension model)
          feats = intercalate "\n" $ map (\(f,v) -> show f ++ ":" ++ show v) (features model)
