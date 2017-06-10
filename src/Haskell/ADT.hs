



module Haskell.ADT where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  intro
  patternMatching
  ex1
  caseExpr
  trees

intro :: SlideF String
intro = do
  h "Einführung" green $ do
    p $ do
      l "Betrachten Sie folgende Definition einer Datenstruktur:"
    haskell $ do
      c "data Bool = False"
      c "          | True"
      c "          deriving (Eq, Ord, Show)"
    p $ do
      l "Welche Elemente erkennen Sie?"
    p $ do
      l "Mit" >> cmd ":info"
      l "können Sie sich im" >> ghci
      l "Informationen zu Konstruktoren"
      l "Datentypen und Typklassen ausgeben lassen."

patternMatching :: SlideF String
patternMatching = do
  h "Pattern Matching" green  $ do
    p $ do
      l "Die verschiedenen Konstruktoren eines Datentypes"
      l "können durch Pattern-Matching unterschieden werden."
    haskell $ do
      c "not :: Bool -> Bool"
      c "not True = False"
      c "not False = True"
    p $ do
      l "Variablen im Pattern werden gebunden: "
    haskell $ do
      c "f :: (Bool, Integer) -> String"
      c "f (a, x) = \"Bound a to \" ++ show a"
      c "           ++ \" and x to \" ++ show x"

ex1 :: SlideF String
ex1 = do
  h "Übung" green $ do
    p $ do
      l "Definieren sie die Implikation als Operator"
    haskell $ do
      c "(.->) :: Bool -> Bool -> Bool"
    p $ do
      l "Benutzen Sie _ um das Binden unbenötigter Argumente"
      l "zu vermeiden."
      a "http://de.wikipedia.org/wiki/Implikation#Wahrheitsfunktionale_Implikation"
        "Die entsprechende Werte-Tafel finden Sie hier."
    question $ do
      l "Wieviele Zeilen brauchen Sie?"

caseExpr :: SlideF String
caseExpr = do
  h "case-Ausdrücke" green $ do
    p $ do
      l "Pattern Matching können Sie auch mit einem"
      expr "case ... of" >> l "-Ausdruck verwenden:"
    haskell $ do
      c "not :: Bool -> Bool"
      c "not x = case x of"
      c "             False -> True"
      c "             True -> False"
    p $ do
      l "Achten Sie auf korrekte Einrückung!"
    p $ do
      expr "case"
      l "ist ein Ausdruck, genauso wie"
      expr "let"
      l "."

trees :: SlideF String
trees = do
  h "Rekursive Datenstrukturen" green $ do
    p $ do
      l "Datenstrukturen können rekursiv definiert werden,"
      l "so z.B. die Liste:"
    haskell $ do
      c "data List a ="
      c "  Nil | Cons a (List a) deriving (Show)"
    p $ do
      l "Eine solche rekursive Struktur eignet sich"
      l "natürlich hervorragend dazu, mit einer rekursiven"
      l "Funktion durchlaufen zu werden."
    p $ do
      em "Übung:"
    p $ do
      l "Definieren Sie einen binären Baum" >> expr "Tree a" >> l "."
      l "Definieren Sie eine Funktion" >> expr "nodeCount"
      l ", die die Knoten eines Baums zählt."
      l "Definieren Sie" >> expr "leafCount"
      l ", um die Blätter zu zählen."
