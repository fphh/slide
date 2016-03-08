

module Haskell.Function where


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  expressions
  expressionsII
  functions
  letExpr
  ghciTips
  lambda
  operators
  operatorsII

  
expressions :: SlideF String
expressions = do
  h "Ausdrücke" darkblue $ do
    p $ do
      l "Alles in Haskell ist ein Ausdruck."
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
      l "Mit" >> cmd ":t" >> l "können Sie im" >> ghci
      l "den Typen eines Ausdrucks erfragen:"
    haskell $ do
      c "> :t 5"
      c "5 :: Num a => a"
    p $ do
      l "Lassen Sie sich die Typen der aufgeführten Ausdrücke"
      l "ausgeben."

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

  
functions :: SlideF String
functions = do
  h "Funktionsausdrücke" darkblue $ do
    p $ do
      l "Funktionen sind ebenfalls Ausdrücke."
      l "Sie können auf zwei Weisen geschrieben werden:"
    haskell $ do
      c "> let f x y = x + y"
      c "> f 1 2"
      c "3"
    p $ l "oder äquivalent:"
    haskell $ do
      c "> let g = \\x y -> x + y"
      c "> g 1 2"
      c "3"

letExpr :: SlideF String
letExpr = do
  h "Let ... in ..." darksalmon $ do
    p $ do
      l "Mit"
      expr "let ... in ..."
      l "bzw. einfach" >> expr "let ..." 
      l "können Sie im GHCi Funktionen und Ausdrücke definieren."
    p $ do
      l "Für Definitionen in einer Datei"
      l "brauchen Sie kein" >> expr "let" >> l "."
    p $ do
      l "Definieren Sie eine Funktion"
      l "sowohl im" >> ghci
      l "als auch in einer Datei."
{-
    p $ do
      l "Datein können Sie mit" >> cmd ":load <file>"
      l "bzw." >> cmd ":reload" >> l "im" >> ghci
      l "laden. Auch die Kurzformen"
      cmd ":l" >> l "und" >> cmd ":r" >> l "gibt es."
-}

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
      expr "(:)" >> l "."
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
