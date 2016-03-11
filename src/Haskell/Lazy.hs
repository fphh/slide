

module Haskell.Lazy where

import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  lazyIntro
  evalStrat
  evalStratII
  lazyEval
  lazyEvalII
  
lazyIntro :: SlideF String
lazyIntro = do
  h "Lazy Evaluation" lightgreen $ do
    p $ do
      l "Eine reducible expression, kurz"
      em "Redex" >> l ","
      l "ist ein Funktion zusammen mit ihren Argumenten,"
      l "so dass die Funktion angewendet werden kann."
    p $ do
      l "Gegeben sei:"
    haskell $ do
      c "mult :: Int -> Int -> Int"
      c "mult x y = x * y"
    p $ do
      l "Welche Redexs erkennen Sie in folgendem Ausdruck:"
    haskell $ do
      c "> mult (1 + 2) (2 + 3)"
    p $ do
      l "Ist kein Redex vorhanden, ist der Ausdruck in Normalform."

evalStrat :: SlideF String
evalStrat = do
  h "Evaluierungsstrategien" lightgreen $ do
    p $ do
      l "Zwei Evaluierungsstrategien sind üblich:"
    p $ do
      em "Outermost:" >> def "Immer der äusserste Redex wird reduziert."
    p $ do
      em "Innermost:" >> def "Immer der innerste Redex wird reduziert."
    p $ do
      l "Gibt es zwei oder mehr äusserste/innerste Redexe,"
      l "beginnt man von links."

    p $ do
      em "Innermost" >> l "wird auch" >> em "call-by-value" >> l "genannt."
    p $ do
      em "Outermost" >> l " wird auch" >> em "call-by-name" >> l " genannt."

evalStratII :: SlideF String
evalStratII = do
  h "Evaluation by Hand" lightgreen $ do
    p $ l "Bringen Sie die zweite Zeile in Normalform:"
    haskell $ do
      c "> let square x = x * x"
      c "> square (1 + 2)"
    p $ do
      l "Wieviele Reduktionsschritte brauchen Sie jeweils?"
    p $ do
      l "Werten sie die Ausdrücke" >> expr "[1..]"
      l "und" >> expr "const [1..] 5" >> l "im"
      ghci >> l "aus. Mit" >> em "Strg-C"
      l "können Sie eine Berechnung unterbrechen."
    question $
      l "Welche Strategie wendet Haskell an?"


lazyEval :: SlideF String
lazyEval = do
  h "Lazy Evaluation" lightgreen $ do
    p $ do
      l "Haskell verwendet" >> em "Lazy Evaluation" >> l ", d.h:"
    p $ do
      l "Die Strategie" >> em "outermos" >> l "zusammen mit"
      em "sharing" >> l "."
    p $ do
      em "Sharing" >> l "bedeutet, dass Ausdrücke, die an die"
      l "gleiche Variable gebunden sind, nur einmal ausgewertet"
      l "werden. Solange ein solcher Redex nicht ausgewertet"
      l "wurde, nennt man ihn" >> em "Thunk" >> l "."
      em "Thunks" >> l ", die für das Ergebnis nicht benötigt werden"
      l "müssen, bleiben unausgewertet."
    p $ do
      l "Werten Sie" >> expr "square (1+2)"
      l "mit dieser Strategie nocheinmal aus."

lazyEvalII :: SlideF String
lazyEvalII = do
  h "Lazy Evaluation II" darkcyan $ do  
    p $ do
      l "Haskell erreicht damit maximale Definiertheit"
      l "bei minimaler Anzahl von Auswertungsschritten."
    p $ do
      l "Lazy Evaluation erlaubt es, den Algorithmus von"
      l "von der Kontrolle zu trennen:"
    haskell $ do
      c "> let xs = iterate (\\x -> 2*x+3) 0"
      c "> xs !! 5"
      c "93"
      c "takeWhile (< 500) xs"
      c "[0,3,9,21,45,93,189,381]"
