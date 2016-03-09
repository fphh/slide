

module Haskell.DotAndDollar where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  dot
  dollar
  verification
  currying
  curryingEx
  functionTypes
  functionTypesII
  
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
      
verification :: SlideF String
verification = do
  h "Programm-Verifikation mit (.) und ($)" darkorange $ do
    p $ do
      l "Beweisen Sie:"
    pcenter $
      expr "(f . g) x = f $ g x"
    p $ do
      l "Hinweise:"
    p $ do
      l "*" <| [ marginL 20] >> l "Benutzen Sie Bleistift und Papier."
    p $ do
      l "*" <| [ marginL 20] >> l "Funktionsgleichungen können Sie umformen:"
    pcenter $ expr "f x = ... ⇔ f = \\x -> ..."
    p $ do
      l "*" <| [ marginL 20] >> l "Für eine Variable, z.B." >> expr "x"
      l ", die nicht in" >> expr "f" >> l "vorkommt:"
    pcenter $ expr "f = \\x -> f x"

currying :: SlideF String
currying = do
  h "Funktionstypen und Currying" lightblue $ do
    p $ do
      l "Folgender Funktionstyp sei gegeben:"
    haskell $ c "f :: Int -> Int -> Int -> Int"
    p $ do
      l "Der Typoperator" >> expr "->"
      l "ist rechtsassoziativ, also"
    haskell $ c "f :: Int -> (Int -> (Int -> Int))"
    p $ do
      expr "f"
      l "ist eine Funktion, die ein"
      expr "Int"
      l "nimmt, und eine Funktion vom Typen"
      expr "Int -> (Int -> Int)"
      l "zurückgibt:"
    haskell $ do
      c "> :t (f 8)"
      c "f 8 :: Int -> Int -> Int"

curryingEx :: SlideF String
curryingEx = do
  h "Currying Übungen" lightblue $ do
    p $ do
      l "Addieren Sie zu jeder Zahl in einer Liste 1"
      l "und multiplizieren Sie das Ergebnis mit 2."
    p $ do
      l "Definieren Sie dazu eine Funktion, die zwei"
      l "Zahlen addiert und eine, die zwei Zahlen multipliziert."
    p $ do
      l "Benutzen Sie diese Funktionen zusammen mit den Funktionen"
      lib "map" >> l "und" >> lib "(.)"
      l "."

functionTypes :: SlideF String
functionTypes = do
  h "Funktionstypen Übungen" lightblue $ do
    p $ l "Schreiben Sie Aufgaben mit folgenden Typen:"
    haskell $ do
      c "f :: (Int -> Int) -> Int -> Int"
      c "g :: (a -> b) -> a -> a"
      c "h :: a -> (a -> Char) -> a"
      c "i :: (a -> b) -> a -> b"
      
functionTypesII :: SlideF String
functionTypesII = do
  h "Funktionstypen Übungen II" lightblue $ do
    p $ do
      l "Machen Sie sich mit der"
      lib "flip" >> l "vertraut."
      l "Erfragen Sie deren Typen im GHCi und interpretieren Sie diesen."
    p $ do
      l "Implementieren Sie" >> expr "h" >> l "mit Hilfe von"
      expr "g" >> l "und" >> lib "flip" >> l "."
    question $ do
      l "Geht es anders herum?"
    p $ do
      l "Implementieren Sie" >> expr "g"
      l "mit Hilfe von" >> lib "flip"
      l "und" >> expr "const"
      l "."
    p $ do
      l "Implementieren Sie folgende Funktion"
    haskell $ c "j :: a -> b"

