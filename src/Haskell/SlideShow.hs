

module Haskell.SlideShow where

import Slide

import qualified Haskell.Intro as Intro
import qualified Haskell.Recursion as Recursion
import qualified Haskell.DifferenceEngine as DifferenceEngine
import qualified Haskell.Function as Function
import qualified Haskell.DotAndDollar as DotAndDollar
import qualified Haskell.FunctionTypes as FunctionTypes 
import qualified Haskell.ADT as ADT
import qualified Haskell.Interpreter as Interpreter
import qualified Haskell.FunctorAndApplicative as FunctorAndApplicative
import qualified Haskell.Monad as Monad
import qualified Haskell.Lazy as Lazy
import qualified Haskell.Eratosthenes as Eratosthenes
import qualified Haskell.Verification as Verification

import Haskell.Title (titlePage, theEnd)

slideShow :: SlideF String
slideShow = slideShow5

slideShow1 :: SlideF String
slideShow1 = do
  titlePage
  Intro.slideShow
  Function.slideShow
  DotAndDollar.slideShow
  theEnd

slideShow2 :: SlideF String
slideShow2 = do
  titlePage
  Recursion.slideShow
  theEnd

slideShow3 :: SlideF String
slideShow3 = do
  titlePage
  DifferenceEngine.slideShow
  Lazy.slideShow
  Eratosthenes.slideShow
  theEnd

slideShow4 :: SlideF String
slideShow4 = do
  titlePage
  Verification.slideShow
  theEnd

slideShow5 :: SlideF String
slideShow5 = do
  titlePage
  ADT.slideShow
  Interpreter.slideShow
  FunctionTypes.slideShow
  theEnd


slideShow6 :: SlideF String
slideShow6 = do
  titlePage
  FunctorAndApplicative.slideShow
  Monad.slideShow
  theEnd
