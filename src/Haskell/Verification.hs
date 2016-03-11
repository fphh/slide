

module Haskell.Verification where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  verifyDotAndDollar  
  dotAndDollar
  flipCurry
  induction
  inductionProof
  ex1
  ex2
  
verifyDotAndDollar :: SlideF String
verifyDotAndDollar = do
  h "Programm-Verifikation mit (.) und ($)" darkorange $ do
    p $ do
      l "Beweisen Sie:" >> expr "(f . g) x = f $ g x"
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
    p $ do
      l "Jeder Zwischenschritt muss gültiger Ausdruck"
      l "in Haskell sein."
      
dotAndDollar :: SlideF String
dotAndDollar = do
  h "Widerholung (.) und ($)" darkorange $ do
    p $ do
      l "Zur Erinnerung:"
    haskell $ do
      c "(.) :: (b -> c) -> (a -> b) -> a -> c"
      c "(.) f g = \\x -> f (g x)"
    haskell $ do
      c "($) :: (a -> b) -> a -> b"
      c "f $ x = f x"

flipCurry :: SlideF String
flipCurry = do
  h "Flip und Curry" darkorange $ do
    p $ do
      l "Zeigen Sie"
    pcenter $ expr "curry snd = flip const"
    p $ do
      l "mit Hilfe folgender Definitionen:"
    haskell $ do
      c "curry f x y = f (x, y)"
      c "snd (_,y) = y"
      c "flip f x y = f y x"
      c "const x _ = x"
    p $ do
      l "Überlegen Sie sich die Typen und"
      l "überprüfen Sie mit" >> expr ":t" >> l "."

induction :: SlideF String
induction = do
  h "Induktionsbeweis" red $ do
    p $ do
      l "Wir verarbeiten rekursive Datenstrukturen"
      l "mit Hilfe rekursiver Funktionen."
    p $ do
      l "Mittels Induktion lassen sich Eigenschaften"
      l "rekursiver Funktionen beweisen."
    p $ do
      l "Ein Induktionsbeweis besteht aus"
    p $ do
      def "*" <| [marginL 20] >> def "einem Basisfall und"
    p $ do
      def "*" <| [marginL 20] >> def "einem rekursiven Fall."

inductionProof :: SlideF String
inductionProof = do
  h "Induktionsprinzip" red $ do
    p $ do
      em "Beispiel:" >> l "Wir versuchen die Hypothese"
    pcenter $ expr "p(n) = (n < n+2)"
    p $ do
      l "für alle ganzen Zahlen" >> expr "n" >> l "zu beweisen."
    p $ do
      l "Zunächst beweisen wir den Basisfall"
      expr "p(0)," >> l "also"
      expr "p(0) = 0 < 2"
      l "."
    p $ do
      l "Anschließend beweisen wir" >> expr "p(n+1)" >> l ":"
    p $ do
      expr "p(n+1) = (n+1 < n+3) = (n < n+2) = p(n)"
    p $ do
      expr "p(n+1)" >> l "hat den gleichen Wahrheitswert wie"
      expr "p(n)" >> l "."
    p $ do
      l "Gemäß Hypothese ist"
      expr "p(n)" >> l "wahr."
      l "Also ist auch" >> expr "p(n+1)" >> l "wahr."

ex1 :: SlideF String
ex1 = do
  h "Übung Induktionsbeweis I" darkorange $ do
    p $ do
      l "Beweisen Sie"
    pcenter $ expr "length (replicate n x) = n"
    p $ do
      l "mit Hilfe folgender Definitionen:"
    haskell $ do
      c "replicate :: Int -> a -> [a]"
      c "replicate 0 _ = []"
      c "replicate n x = x : replicate (n-1) x"
    haskell $ do      
      c "length :: [a] -> Int"
      c "length [] = 0"
      c "length (x:xs) = 1 + length xs"

ex2 :: SlideF String
ex2 = do
  h "Übung Induktionsbeweis II" darkorange $ do
    p $ do
      l "Beweisen Sie:"
    pcenter $ expr "map f . map g = map (f . g)"
    p $ do
      l "Mit Hilfe folgender Definitionen:"
    haskell $ do
      c "(f . g) :: (b -> c) -> (a -> b) -> (a -> c)"
      c "(f . g) x = f (g x)"
    haskell $ do      
      c "map :: (a -> b) -> [a] -> [b]"
      c "map _ [] = []"
      c "map f (x:xs) = f x : map f xs"
