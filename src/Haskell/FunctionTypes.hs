

module Haskell.FunctionTypes where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  currying
  curryingEx
  functionTypes
  functionTypesII

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

