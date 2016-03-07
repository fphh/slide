

module Haskell.SlideShow where

import Slide

import qualified Haskell.Intro as Intro
import qualified Haskell.Recursion as Recursion
import qualified Haskell.DifferenceEngine as DE

import Haskell.Title (titlePage, theEnd)


slideShow :: SlideF String
slideShow = do
  titlePage
  -- Intro.slideShow
  -- Recursion.slideShow
  DE.slideShow
  theEnd
