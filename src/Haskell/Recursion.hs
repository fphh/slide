

module Haskell.Recursion where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  -- chapter "Rekursion über" "Ganze Zahlen und Listen" seagreen
  naturalNumbers
  recursiveFunction
  ex1a
  ex1b
  ex2
  -- chapter "Noch ein rekursiver Datentyp:" "Listen" maroon
  lists
  listsExample
  listsPatternMatching
  listsPatternMatchingII
  exListsI
  exListsII
  exListsIII


naturalNumbers :: SlideF String
naturalNumbers = do
  header "Ganze Zahlen" seagreen $ do
    p $ do
      l "Betrachten Sie folgende Definition:"
    p $ def "· 0 ist eine ganze Zahl."
    p $ def "· Wenn n eine ganze Zahl ist, dann ist (n+1) eine ganze Zahl."
    p $ do
      l "Die ganzen Zahlen sind induktiv definiert."
      l "Rekursion bedeutet:"
    p $ def "· Wir haben einen Basisfall."
    p $ do
      def "· Wir haben einen Fall,"
      def "der sich auf einen kleineren Fall zurückführen läßt."
    p $ do
      l "Induktive Strukturen eignen sich zur Verarbeitung"
      l "mittels Rekursion."

recursiveFunction :: SlideF String
recursiveFunction = do
  header "Eine rekursive Funktion" seagreen $ do
    p $ l "Zum Beispiel:"
    haskell $ do
      c "sumN :: Integer -> Integer"
      c "sumN 0 = 0"
      c "sumN n = n + sumN (n-1)"
    question $ do
      l "Welche Programmelemente erkennen Sie in dieser Definition?"
      l "Was berechnet diese Funktion?"


      
ex1a :: SlideF String
ex1a = do
  header "Übung Ia" darkblue $ do
    p $ do
      l "Öffnen Sie eine Datei" >> em "Numbers.hs"
      l "und implementieren Sie die Fakultätsfunktion:"
    haskell $ do
      c "prod :: Integer -> Integer"
    p $ l "so dass"
    (p $ def "prod n = 1 * 2 * ... * n") <| [center]
    p $ do
      l "Beginnen Sie mit der Klausel für den Basisfall."
      l "Erweitern Sie die Definition dann um den rekursiven Fall."

ex1b :: SlideF String
ex1b = do
  header "Übung Ib" darkblue $ do
    p $ l "Laden Sie Ihre Datei in den ghci:"
    haskell $ do
      c "$ ghci Numbers.hs"
      c "> prod 3"
      c "6"
    question $ do
      l "Testen Sie mit großen Zahlen."
      l "Testen Sie mit negativen Zahlen. Was passiert?"


ex2 :: SlideF String
ex2 = do
  header "Übung II" blue $ do
    p $ do
      l "Implementieren Sie"
      a "http://de.wikipedia.org/wiki/Fibonacci-Folge#Definition_der_Fibonacci-Folge"
        "Fibonacci"
      l "."
    p $ do
      l "Implementieren Sie den größten gemeinsamen Teiler (gcd) mittels"
      a "http://de.wikipedia.org/wiki/Euklidischer_Algorithmus#Rekursive_Variante" "Euklid"
      l "."
    question $ do
      l "Welche Signatur hat fibonacci?"
      l "Welche Signatur hat gcd?"


lists :: SlideF String
lists = do
  header "Listen" maroon $ do
    p $ do
      l "Betrachten Sie folgende Definition"
    p $ def "· [ ] ist die leere Liste."
    p $ do
      def "· Wenn xs eine Liste ist und x ein Element,"
      def "dann ist (x:xs) eine Liste."
    p $ do
      cmd "(:)" >> l "ist der Listenkonstruktor."
      l "Alternativer Schreibweise:"
    pcenter $ l "x:[] ~ [x]" <| [darkgreen, monospace]
    pcenter $ l "x:y:[] ~ [x, y]" <| [darkgreen, monospace]
    pcenter $ l "x:y:z:[] ~ [x, y, z]" <| [darkgreen, monospace]
    pcenter $ l "etc."


listsExample :: SlideF String
listsExample = do
  header "Noch eine rekursive Funktion" maroon $ do
    p $ l "Betrachten Sie folgende Definition:"
    haskell $ do
      c "reverse :: [a] -> [a]"
      c "reverse [] = []"
      c "reverse (x:xs) = reverse xs ++ [x]"
    question $ do
      l "Welche Programmelemente erkennen Sie?"
      l "Was macht diese Funktion?"
    p $ do
      l "Der Append-Operator" >> cmd "(++)"
      l "konkateniert zwei Listen."
    haskell $ do
      c "> [1, 2] ++ [3, 4, 5]"
      c "[1, 2, 3, 4, 5]"

listsPatternMatching :: SlideF String
listsPatternMatching = do
  h "Pattern-Matching für Listen" darkred $ do
    p $ do
      l "Durch Pattern-Matching dekomponiert man"
      l "eine Datenstruktur. Für Listen bedeutet das"
      l "eine Zerlegung in" >> em "head"
      l "und" >> em "tail"
      l "."
    haskell $ do
      c "> let (a:as) = [1,2,3]"
      c "> a"
      c "1"
      c "> as"
      c "[2,3]"
    p $ do
      l "Testen Sie Pattern-Matching im" >> ghci >> l "."
    question $ do
      l "Was, wenn Sie zwei Kopfelemente abspalten wollen?"


listsPatternMatchingII :: SlideF String
listsPatternMatchingII = do
  h "Pattern-Matching in Parametern" darkred $ do
    p $ do
      l "Auch beim Aufrufen von Funktionen kann Pattern-Matching"
      l "nützlich sein:"
    haskell $ do
      c "f :: [Integer] -> Integer"
      c "f (x:y:z:_) = x+y+z"
    p $ do
      l "Der Unterstrich _ steht für" >> em "don't-care"
      l "-Argumente, also Teile einer Datenstruktur,"
      l "die rechts vom Geleichheitszeichen nicht"
      l "benötigt werden."
    question $ do
      l "Was, wenn die Liste nicht die erwartete"
      l "Anzahl von Elementen hat?"
    p $ do
      l "Testen Sie im" >> ghci >> l "."


exListsI :: SlideF String
exListsI = do
  header "Übungen zu Listen" crimson $ do
    p $ l "Schreiben Sie eine Funktion"
    haskell $ c "toList :: Integer -> [Integer]"
    p $ l "so dass"
    (p $ def "toList n = [1,1, ..., 1] -- n mal 1") <| [center]
    p $ do
      l "Beginnen Sie mit dem Basisfall,"
      l "dann implementieren Sie den rekursiven Fall."


exListsII :: SlideF String
exListsII = do
  header "Übungen zu Listen" crimson $ do
    p $ do
      l "Schreiben Sie die Umkehrfunktion:"
    haskell $ do
      c "fromList :: [Integer] -> Integer"
    p $ do
      l "Überlegen Sie sich einen Test für diese beiden Funktionen"
      l "mit Hilfe des Vergleichsoperators" >> cmd "(==)"
      l "."
    p $ do
      l "Implementieren Sie diesen Test."


exListsIII :: SlideF String
exListsIII = do
  header "Übungen zu Listen" crimson $ do
    p $ do
      l "Schreiben Sie eine Funktion" >> cmd "mapF"
      l "die eine Funktion auf jedes Element der Liste anwendet."
      l "Zum Beispiel:"
    haskell $ do
      c "> mapF toList [2,3,0,1]"
      c "[[1,1],[1,1,1],[],[1]]"
      c "> mapF fromList it"
      c "[2,3,0,1]"
    p $ do
      l "Die Variable it enthält immer das letzte Ergebnis."
    question $ do
      l "Welchen Typ hat mapF?"
    p $ do
      cmd "mapF" >> l "ist auch als Bibliotheksfunktion"
      lib "map" >> l "im" >> ghci >> l "verfügbar."
      
