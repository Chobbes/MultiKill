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

import MultiKill.Model
import MultiKill.Parser

import Data.Attoparsec.Text hiding (take)
import Data.List
import Data.Maybe
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath
import System.Random


main = do (numStr:outputFileName:fileNames) <- getArgs
          let num = read numStr

          modelFiles <- mapM T.readFile fileNames
          let allModels = map (takeRight . parseOnly parseModel) modelFiles
          
          gen <- getStdGen
          let models = takeRandom gen num allModels

          createDirectoryIfMissing True (takeDirectory outputFileName)
          writeFile outputFileName (show $ modelAverage models)
  where takeRight (Right a) = a
        takeRight (Left err) = error ("Could not parse model!\n" ++ err)

modelAverage :: [Model] -> Model
modelAverage models = (head models) {features = map avg keys}
  where feats = map features models
        keys = map fst (head feats)
        avg key = (key, (sum . catMaybes $ map (lookup key) feats) / (fromIntegral $ length models))

takeRandom :: RandomGen g => g -> Int -> [a] -> [a]
takeRandom _ 0 _ = []
takeRandom _ _ [] = error "Called on empty list!"
takeRandom gen num xs
         | num > length xs = error "Out of range!"
         | otherwise = value : (takeRandom newGen (num - 1) rest)
  where (index, newGen) = randomR (0, (length xs) - 1) gen
        (start, value, end) = (take index xs, xs !! index, drop (index + 1) xs)
        rest = start ++ end
