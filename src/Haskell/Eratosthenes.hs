

module Haskell.Eratosthenes where

import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  sieveExerciseI
  sieveExerciseII

sieveExerciseI :: SlideF String
sieveExerciseI = do
  h "Übung: Sieb des Eratosthenes" green $ do
    p $ l "Das Sieb des Eratosthenes filtert alle Nicht-Primzahlen:"
    haskell $ do
      c "sieve :: [Integer] -> [Integer]"
    p $ l "Filtern Sie zunächst alle Vielfachen einer Zahl heraus:"
    haskell $ do
      c "filterMultiples ::"
      c "    Integer -> [Integer] -> [Integer]"
    p $ do
      l "Implementieren Sie diese Funktion mit" >> lib "mod"
      l "und" >> lib "filter" >> l "."
   
sieveExerciseII :: SlideF String
sieveExerciseII = do
  h "Übung: Sieb des Eratosthenes" green $ do
    p $ do
      l "Implementieren Sie"
      func "sieve"
      l ", indem Sie das erste Element behalten"
      l "und aus dem Rest der Liste alle Vielfachen herrausfiltern."
      l "Auf den gefilterten Rest der Liste wenden Sie wiederum"
      func "sieve" >> l "an."    
    p $ do
      l "Definieren Sie:"
    haskell $ do
      c "primes :: [Integer]"
      c "primes = sieve [2..]"
    p $ do
      l "Geben Sie mit"
      lib "take"
      l "die ersten 100 Primzahlen aus."
    p $ do
      l "Schreiben Sie eine Funktion,"
      l "die die" >> expr "n"
      l "-te Priemzahl ausgibt."
