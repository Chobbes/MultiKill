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

{-# LANGUAGE OverloadedStrings #-}

module MultiKill.Parser (parseModel) where

import MultiKill.Model

import Control.Applicative
import Data.Attoparsec.Text
import Prelude hiding (takeWhile)


-- | Parse PSSP model.
parseModel :: Parser Model
parseModel = do string "m:"
                tps <- decimal
                skipToEndOfLine
                
                timeInterval <- parseTimeInterval
                skipToEndOfLine
                
                string "r:"
                r <- decimal
                skipToEndOfLine
                
                string "DIM:"
                dim <- decimal
                
                feats <- many parseKeyInt

                skipSpace
                endOfInput
                
                return (Model tps timeInterval r dim feats)

-- | Parse the comma separated time points for the interval.
parseTimeInterval :: Parser [Double]
parseTimeInterval = 
  do point <- double
     others <- (do char ',' *> parseTimeInterval) <|> (return [])
     return (point : others)

-- | Parse a key:int pair for model weights.
parseKeyInt :: Parser (Integer, Double)
parseKeyInt = do key <- decimal
                 char ':'
                 value <- double
                 skipToEndOfLine
                 return (key, value)

-- | Newline parser to handle all sorts of horrible.
endOfLine' :: Parser ()
endOfLine' = endOfLine <|> endOfInput <|> (char '\r' *> return ())

-- | Skip to the next line...
skipToEndOfLine :: Parser ()
skipToEndOfLine = takeWhile (\c -> c /= '\n' && c /= '\r') >> endOfLine'
