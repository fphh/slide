

module Haskell.Function where


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  expressions
  expressionsII
  letExpr
  letExprII
  functions
  operators
  operatorsII
  lambda
  lambdaII
  ghciTips

  
expressions :: SlideF String
expressions = do
  h "Ausdrücke" darkblue $ do
    p $ do
      l "Ein Haskell-Programm besteht aus"
      em "Ausdrücken."
      l "Jedem Ausdruck ist ein "
      em "Typ"
      l "zugeordnet."
    p $ do
      l "Einfache Ausdrücke sind z. B. Zahlen, Buchstaben,"
      l "Zeichenketten oder boolsche Ausdrücke:"
    p $ do
      expr "5" >> l ", "
      expr "3.1415" >> l ", "
      expr "'a'" >> l ", "
      expr "\"abc\"" >> l ", "
      expr "True" >> l "."
    p $ do
      cmd ":t" >> l "gibt im" >> ghci
      l "den Typen eines Ausdrucks zurück:"
    haskell $ do
      c "> :t 5"
      c "5 :: Num a => a"
    p $ do
      l "Ermitteln Sie die Typen der aufgeführten Ausdrücke."

expressionsII :: SlideF String
expressionsII = do
  h "Zusammengesetze Ausdrücke" darkblue $ do
    p $ do
      l "Einfache Ausdrücke lassen sich zu komplexeren"
      l "Ausdrücken zusammensetzen, z. B.:"
    p $ do
      expr "1+2" >> l ", "
      expr "1:2:3:[]" >> l ", "
      expr "1 == 2" >> l ", "
      expr "True && False" >> l "."
    p $ do
      l "Lassen Sie sich die Typen der aufgeführten Ausdrücke mit"
      expr ":t"
      l "ausgeben."
    p $ do
      expr "[]"
      l "ist die leere Liste und"
      expr "(:)"
      l "ist der" >> em "cons"
      l "-Operator, der ein Element vorne in eine"
      l "Liste einfügt."
      l "So entsteht eine Liste, die ein Element länger ist."

letExpr :: SlideF String
letExpr = do
  h "Let ... in ..." darksalmon $ do
    p $ do
      l "Mit"
      expr "let ... in ..."
      l "können Sie Ausdrücke und Funktionen definieren und verwenden:"
    haskell $ do
      c "> let a = 3; b = 7 in 2*(a+b)"
      c "20"
      c "> let f x = 2*x in f 3"
      c "6"
    p $ do
      expr "let .. in ..."
      l "ist selbst ein Ausdruck:"
    haskell $ do
      c "> 1 + (let a = 3 in 2*a)"
      c "7"


letExprII :: SlideF String
letExprII = do
  h "Let ... in ... (Teil II)" darksalmon $ do
    p $ do
      l "Innerhalb eines" >> expr "let"
      l "ist die Reihenfolge der Definitionen egal:"
    haskell $ do
      c "let a = b; b = 3 in a"
      c "let b = 3; a = b in a      -- äquivalent"
    p $ do
      l "Auch rekursive Definitionen sind erlaubt:"
      
    haskell $ do
      c "> let zeros = 0 : zeros in zeros"
      c "[0,0,0,0,Interupted."
    p $ do
      l "Mit Strg-C brechen Sie den unendlichen Stream ab."
      
                          
functions :: SlideF String
functions = do
  h "Let ohne in" darksalmon $ do
    p $ do
      l "Funktionen und Ausdrücke können im" >> ghci >> l "auch ohne"
      expr "in"
      l "definiert werden."
    haskell $ do
      c "> let f x y = x + y"
      c "> let a = 2"
      c "> f 1 a"
      c "3"
    p $ do
      l "Für Top-Level-Definitionen in einer Datei"
      l "brauchen Sie kein" >> expr "let" >> l "."
      
      

lambda :: SlideF String
lambda = do
  h "Lambda-Ausdrücke" darkblue $ do
    p $ do
      l "Die Schreibweise"
      expr "\\... -> ..."
      l "wird als Lambda-Ausdruck bezeichnet."
      l "Lambda-Ausdrücke können überall verwendet werden,"
      l "wo Funktionen stehen müssen, z.B.:"
    haskell $ do
      c "> (\\x -> x+1) 10"
      c "11"
    p $ do
      l "Schreiben Sie im" >> ghci >> l "eine Anwendung"
      l "eines Lambda-Ausdrucks auf zwei Zahlen."
    p $ do
      l "Definieren Sie eine Funktion, die eine"
      l "andere Funktion und ein Argument nimmt"
      l "und diese Funktion auf das Argument anwendet."

operators :: SlideF String
operators = do
  h "Operatoren" darkblue $ do
    p $ do
      l "Operatoren sind Funktionen,"
      l "die zwei Argumente nehmen:"
    p $ do
      expr "(+)" >> l ","
      expr "(^)" >> l ","
      expr "(&&)" >> l ","
      expr "(||)" >> l ","
      expr "(/=)" >> l ","
      expr "(:)" >> l ","
      expr "(,)" >> l "..."
    p $ do
      l "Wenn Sie Operatoren in" >> em "infix"
      l "-Schreibweise verwenden, können Sie die Klammern"
      l "weglassen:"
    haskell $ do
      c "> 2*3"
      c "6"
      c "> (*) 2 3"
      c "6"
    p $ do
      l "Lassen Sie sich die Typen der aufgeführten"
      l "Funktionen ausgeben."

lambdaII :: SlideF String
lambdaII = do
  h "Lambda-Ausdrücke (Teil II)" darkblue $ do
    p $ do
      l "Funktionsdefinitionen könne auch mit Lambda-Ausdrücken"
      l "vorgenommen werden."
      l "Alle vier Definitionenen sind identisch:"
    haskell $ do
      c "> let f x y = x + y"
      c "> let f x = \\y -> x + y"
      c "> let f = \\x y -> x + y"
      c "> let f = (+)"
    p $ do
      l "Der Schrägstrich \\ steht für den griechischen"
      l "Buchstaben Lambda."
    p $ do
      l "Der Funktionskörper steht nach dem Pfeil (->)."


operatorsII :: SlideF String
operatorsII = do
  h "Funktionen mit zwei Argumenten" darkblue $ do
    p $ do
      l "Zwei-argumentige Funktionen können Sie auch in"
      l "Operator-Schreibweise notieren, so zum Beispiel"
      lib "mod" >> l "und"
      lib "rem" >> l "(Modulo und Reminder)."
    haskell $ do
      c "> mod 5 3"
      c "2"
      c "> 5 `mod` 3"
      c "2"
    p $ do
      l "Testen Sie" >> lib "rem" >> l "im" >> ghci >> l "."

ghciTips :: SlideF String
ghciTips = do
  header "GHCi Tips" darkorange $ do
    p $ l "Im" >> ghci
    p $ do
      l "Falls sie eine neue Datei laden wollen,"
      l "dann mit" >> cmd ":load" >> l "oder" >> cmd ":l"
      l "."
    pcode Haskell $ c "> :l NeueDatei.hs"
    p $ do
      l "Falls sie eine Datei erneut laden wollen,"
      l "dann mit" >> cmd ":reload" >> l "oder" >> cmd ":r"
      l "."
    pcode Haskell $ c "> :r"

