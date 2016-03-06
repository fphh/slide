

module Haskell.SlideShow where

import Slide

import qualified Haskell.Intro as Intro
import qualified Haskell.Recursion as Recursion

import Haskell.Title (titlePage, theEnd)


slideShow :: SlideF String
slideShow = do
  titlePage
  Intro.slideShow
  Recursion.slideShow
  theEnd
