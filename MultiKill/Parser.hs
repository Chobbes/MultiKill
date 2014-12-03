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
                endOfLine'
                
                timeInterval <- parseTimeInterval
                endOfLine'
                
                string "r:"
                r <- decimal
                endOfLine'
                
                string "DIM:"
                dim <- decimal
                endOfLine'

                feats <-  many parseKeyWeight
                many endOfLine'
                endOfInput <?> "End of input not reached. Probably some bad padding at the end, or bad features."

                return (Model tps timeInterval r dim feats)

-- | Parse the comma separated time points for the interval.
parseTimeInterval :: Parser [Double]
parseTimeInterval = 
  do point <- double
     others <- (do char ',' *> parseTimeInterval) <|> (return [])
     return (point : others)

-- | Parse a key:double pair for model weights.
parseKeyWeight :: Parser (Integer, Double)
parseKeyWeight = do key <- decimal
                    char ':'
                    value <- double
                    endOfLine'
                    return (key, value)

-- | Newline parser to handle all sorts of horrible.
endOfLine' :: Parser ()
endOfLine' = endOfLine <|> (char '\r' *> return ())
