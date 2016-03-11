

module Haskell.DotAndDollar where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  dot
  dollar
  
dot :: SlideF String
dot = do
  h "Der (.)-Operator" darkorange $ do
    p $ do
      l "Der Operator" >> l "(.)"
      l "komponiert zwei Funktionen:"
    haskell $ do
      c "(.) :: (b -> c) -> (a -> b) -> a -> c"
      c "(.) f g = \\x -> f (g x)"
    p $ do
      l "Gleiche Buchstaben müssen mit gleichen"
      l "Typen belegt werden."      
    haskell $ do
      c "> let f x = x+3"
      c "> let g x = 7*x"
      c "> let h = g . f"
      c "> h 10"
      c "91"

dollar :: SlideF String
dollar = do
  h "Der ($)-Operator" darkorange $ do
    p $ do
      l "Der Operator" >> lib "($)"
      l "wendet eine Funktion auf ein Argument an."
    haskell $ do
      c "($) :: (a -> b) -> a -> b"
      c "f $ x = f x"
    p $ do
      l "Er eignet sich, um Klammern zu sparen."
    haskell $ do
      c "inc :: Integer -> Integer"
      c "inc x = x+1"
    haskell $ do
      c "x, y :: Integer"
      c "x = inc (inc (inc 1))"
      c "y = inc $ inc $ inc 1  -- äquivalent zu x"
      
