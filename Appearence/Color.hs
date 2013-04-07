module Appearence.Color where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

getRGBVal :: String->(String,String)
getRGBVal [] = ([],[])
getRGBVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getRGBVal xs),snd(getRGBVal xs))
                      else (x:fst(getRGBVal xs),snd(getRGBVal xs))

--setColorRGB :: String -> ( Int , Int , Int)
setColorRGB xs =  let (r,rs)   = getRGBVal (tail xs)
                    in 
                      let (g, gs) = getRGBVal rs
                        in 
                          let (b,bs) = getRGBVal gs
                            in currentColor $= Color4 (read r) (read g) (read b) 0
 

