

module Haskell.SlideShow where

import Slide

import qualified Haskell.Intro as Intro
import qualified Haskell.Recursion as Recursion
import qualified Haskell.DifferenceEngine as DifferenceEngine
import qualified Haskell.Function as Function
import qualified Haskell.DotAndDollar as DotAndDollar

import Haskell.Title (titlePage, theEnd)


slideShow :: SlideF String
slideShow = do
  titlePage
  Intro.slideShow
  Function.slideShow
  Recursion.slideShow
  DotAndDollar.slideShow
  DifferenceEngine.slideShow
  theEnd
