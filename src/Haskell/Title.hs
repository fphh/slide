
module Haskell.Title where


import Style
import Slide
import Helper


titlePage :: SlideF String
titlePage = do
  slide (word "") $ do
    para "Funktionale Programmierung mit" <| [pt24, center, fwbold]
    pimage "http://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    newline
    para "VHS München im März 2016"  <| [center]



theEnd :: SlideF String
theEnd = do
  h "Vielen Dank" red $ do
    pimage "http://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    para "Not the End!" <| [center, fwbold, pt24]
    
