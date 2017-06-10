

module Haskell.DifferenceEngine where


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  differenceEnginePicture
  deIntro
  polynom
  powerFunc
  deBegin
  algorithm
  diffs
  allZero
  loop
  lastSteps


differenceEnginePicture :: SlideF String
differenceEnginePicture = do
  h "Differenzenmaschine" red $ do
    p $ do
      l "Charls Babage und Lady Ada im 19. Jahrhundert."
    newline
    pimage "http://upload.wikimedia.org/wikipedia/commons/c/ce/Demonstration_model_of_Babbage%E2%80%99s_Difference_Engine_No_1%2C_19th_century._%289660573663%29.jpg" <| [center, width 300, marginL Auto, marginR Auto]


deIntro :: SlideF String
deIntro = do
  h "Differenzenmaschine" red $ do
    p $ do
      l "Die Differenzenmaschine führt eine Folge von Zahlen fort,"
      l "vorrausgesetzt, die Folge kann durch ein Polynom erzeugt werden."
      l "Zum Beispiel:"
    pcode Haskell $ do
      c "> map (^2) [1, 2, 3, 4]"
      c "[1, 4, 9, 16]"
      c "> next [1, 4, 9, 16]"
      c "25"
    p $ do
      l "..., denn" >> expr "5² = 25"
      l "."
    p $ l "Wir wollen die Funktion" >> func "next" >> l "implementieren!"


polynom :: SlideF String
polynom = do
  h "Was sind Polynome?" red $ do
    p $ l "Polynome sind Funktionen von folgender Form:"
    pcode Haskell $ do
      c "p :: Integer -> Integer"
      c "p x = an * x^n + ... + a1 * x^1 + a0"
    p $ do
      l "Beispiele für Polynome:"
    p $ def "p(x)" <| [marginL 40] >> def "= 2*x³ - 5" <| [marginL 10]
    p $ def "q(x)" <| [marginL 40] >> def " = x² - 7*x¹² + 99" <| [marginL 10]
    p $ do
      l "Der Grad eines Polynoms ist seine höchste Potenz"

powerFunc :: SlideF String
powerFunc = do
  h "Potenzfunktion" darksalmon $ do
    p $ l "Zur Erinnerung die Potenzfunktion:"
    pcode Haskell $ do
      c "(^) :: (Integral b, Num a) => a -> b -> a"
      c "x^0 = 1"
      c "x^1 = 1"
      c "x^n = x * x^(n-1)"

deBegin :: SlideF String
deBegin = do
  h "Der Anfang" red $ do
    p $ do
      l "Öffnen Sie eine neue Datei"
      em "DifferenceEngine.hs"
      l "."
    p $ do
      l "Beginnen Sie diese mit den Zeilen:"
    pcode Haskell $ do
      c "module DifferenceEngine where"
      c "import qualified Data.List as List"
    p $ do
      l "Definieren Sie ein Polynom" >> expr "p"
      l "Ihrer Wahl mit Grad 3"
      l "und eine Liste"
      expr "xs = [p(0), p(1), p(2), p(3), p(4)]."
    p $ do
      l "Benutzen Sie dazu die Funktion" >> lib "map" >> l "."
    p $ do
      l "Vergessen Sie die Signaturen nicht."

algorithm :: SlideF String
algorithm = do
  h "Der Algorithmus" red $ do
    p $ l "Differenzen bilden bis nur noch Nullen auftreten:"
    pcode Haskell $ do
      c "0   1   4   9   16  (Bekannte Ausgangsfolge)"
      c "  1   3   5   7"
      c "    2   2   2"
      c "      0   0"
    p $ l "Die jeweil letzten Zahlen aller Zeilen werden addiert:"
    pcode Haskell $ do
      c "0   1   4   9   16 (0+2+7+16=25)"
      c "  1   3   5   7    (0+2+7=9)"
      c "    2   2   2      (0+2=2)"
      c "      0   0        (0)"

diffs :: SlideF String
diffs = do
  h "Die Funktion diffs" red $ do
    p $ do
      l "Definieren Sie eine Funktion"
    haskell $ do
      c "diffs :: [Integer] -> [Integer]"
    p $ do
      l "die eine Zeile in die nächste überführt."
    p $ do
      l "Was sind geeignete Basisfälle?"
    p $ do
      l "Definieren Sie diff entweder rekursiv"
      l "oder mit Hilfe der Funktionen" >> lib "zipWith"
      l "und" >> lib "tail"
      l "."
    p $ do
      l "Wenn Sie fleißig sind, schreiben Sie beide Versionen."
    p $ do
      l "Testen Sie" >> func "diffs"
      l "mit der von Ihnen definierten Folge"
      expr "xs"
      l "."

allZero :: SlideF String
allZero = do
  h "Die Abbruchbedingung" red $ do
    p $ l "Schreiben Sie ein Prädikat"
    pcode Haskell $ do
      c "allZero :: [Integer] -> Bool"
    p $ do
      l "das prüft, ob eine Liste nur Nullen enthält."
    p $ do
      l "Benützen Sie die Funktion" >> lib "all"
      l "."
    p $ do
      l "Mit hilfe der Funktion" >> lib "not"
      l "erhalten Sie die Negation dieses Prädikats."


loop :: SlideF String
loop = do
  h "Keine Schleife, aber trotzdem" red $ do
    p $ l "Schreiben Sie eine Funktion"
    pcode Haskell $ do
      c "allDiffs :: [Integer] -> [[Integer]]"
    p $ do
      l "so dass das Ergebnis eine Liste aller Differenzenlisten ist."
      l "Dafür benutzen Sie Ihre Funktionen" >> func "diffs"
      l "."
      l "Stoppen Sie, sobald alle Zahlen einer Liste gleich 0 sind."
    p $ do
      l "Benutzen Sie die Funktionen"
      lib "iterate"
      l "und"
      lib "takeWhile"
      l "."


lastSteps :: SlideF String
lastSteps = do
  h "Fast geschafft" darkred $ do
    p $ do
      l "Schreiben Sie die Funktion"
    pcode Haskell $ do
      c "next :: [Integer] -> Integer"
    p $ do
      l "Sie nimmt das Ergebnis von" >> func "allDiffs"
      l "und summiert die jeweil letzten Zahlen."
      l "Benutzen Sie" >> lib "sum" >> l "," >> lib "map"
      l "und" >> lib "last"
      l "."
    p $ do
      l "Prüfen Sie, ob next die nächste Zahl Ihrer Folge"
      l "korrekt vorraussagt:"
    pcode Haskell $ do
      c "> next [p(0), p(1), p(2), p(3), p(4)] == p(5)"
      c "True"
    p $ do
      l "Und fertig!!!" <| [center]

