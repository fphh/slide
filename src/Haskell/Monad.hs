
module Haskell.Monad where


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  monadDefinition
  monadVsApplicative
  doNotation
  monadEx
  inputOutput
  ioEx
  mainFunc
  
monadDefinition :: SlideF String
monadDefinition = do
  h "Monaden" darkorange $ do
    p $ do
      l "Typen der Form" >> expr "m a"
      l "können Ausprägungen der Typklasse"
      expr "Monad" >> l "haben."
    haskell $ do
      c "class Monad m where"
      c "  return :: a -> m a"
      c "  (>>=) :: forall a b. m a -> (a -> m b) -> m b"
    p $ do
      l "Die Typen von" >> expr "Monad" >> l "sollten"
      l "auch Mitglieder von"
      expr "Functor" >> l "und" >> expr "Applicative"
      l "sein."
    p $ do
      l "Beispiele:" >> expr "[a],Maybea,Eitherba,IOa,..."
    p $ do
      l "Dokumentation finden Sie"
      a "http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html"
        "hier"
      l "."

monadVsApplicative :: SlideF String
monadVsApplicative = do
  h "Monaden, mehr als Applicative?" darkorange $ do
    p $ l "Zum Beispiel"
    haskell $ do
      c "> let f x ="
      c "    if x < 4 then Just (2*x) else Nothing"
      c "> Just 3 >>= f"
      c "Just 6"
      c "> Just 4 >>= f"
      c "Nothing"
    question $ do
      l "Können Sie das mit der"
      expr "Applicative" >> l "-Instanz von"
      expr "Maybe" >> l "nachstellen?"


doNotation :: SlideF String
doNotation = do
  h "do-Notation" darkorange $ do
    p $ do
      l "Alles, was eine" >> expr "Monad" >> l "-Instanz hat"
      l "kann auch in" >> expr "do" >> l "-Notaton geschrieben werden:"
    haskell $ do
      c "f :: Integer -> Maybe Integer"
      c "f x = if x < 4 then Just (2*x) else Nothing"
      c ""
      c "test :: Maybe Integer"
      c "test = do"
      c "  v <- Just 3"
      c "  f v"
    
monadEx :: SlideF String
monadEx = do
  h "Übung: Anwendung der Listen-Monade" green $ do
    p $ do
      l "Eine Liste kann als Representation mehrerer Werte"
      l "gleichzeitig verstanden werden. Z.B. sind alle Punkte"
      expr "(x, y)"
      l "im Einheitskreis charakterisiert dadurch, dass"
      expr "x*x + y*y < 1" >> l "."
    p $ do
      l "Berechnen Sie mit der Listen-Monade alle Punkte"
      l "im Einheitskreis. Legen Sie ein Raster in"
      expr "1/10" >> l "-Schritten zugrunde."
    p $ do
      l "Schreiben Sie eine Version mit" >> lib "(>>=)"
      l "und eine mit" >> expr "do" >> l "."
    p $ do
      l "Schreiben Sie eine Version mit"
      lib "liftA2" >> l "."

inputOutput :: SlideF String
inputOutput = do
  h "Input/Output" dodgerblue $ do
    p $ do
      l "Input und Output wird mit Hilfe von"
      em "Actions" >> l "umgesetzt."
    p $ do
      l "Beispiele:"
      lib "getLine" >> l ","
      lib "putStr" >> l ","
      lib "putStrLn" >> l ","
      lib "print" >> l "."
    p $ do
      l "Lassen Sie sich die Typen ausgeben."
    p $ do  
      l "Der Typ" >> expr "IO a"
      l "einer Input- oder Output-" >> em "Action,"
      l "besagt, dass diese bei ihrer Ausführung einen Wert"
      l "vom Typ" >> expr "a" >> l "liefert."
    p $ do
      expr "IO" >> l "ist Instanz von"
      expr "Functor" >> l ", "
      expr "Applicative" >> l "und"
      expr "Monad" >> l "."

ioEx :: SlideF String
ioEx = do
  h "Übung: IO Monade" darkcyan $ do
    p $ do
      l "Schreiben Sie eine" >> em "Action" >> l ","
      l "die zwei Zeilen einließt,"
      l "diese in ganze Zahlen konvertiert"
      l "und deren Summe ausgibt."
    p $ do
      l "Schreiben Sie zunächst eine Action"
      func "getInteger"
      l "mit Hilfe von" >> lib "fmap"
      l "."
    haskell $ do
      c "read :: Read a => String -> a"
    p $ do
      l "Schreiben Sie je eine Implementierung"
      l "mit" >> lib "(>>=)" >> l ","
      l "mit" >> expr "do" >> l "-Notation"
      l "und mit" >> lib "liftA2" >> l "."


mainFunc :: SlideF String
mainFunc = do
  h "Die Funktion main" maroon $ do
    p $ do
      l "Eine besondere Rolle übernimmt die"
      l "Funktion" >> func "main" >> l "."
    haskell $ do
      c "main :: IO ()"
    p $ do
      l "Sie ist der Einstiegspunkt für ein"
      l "kompiliertes Programm."
    p $ do
      l "Definieren Sie eine" >> func "main" >> l "-Funktion,"
      l "indem Sie die vorherige Übung verwenden."
      l "Speichern Sie die Datei unter" >> expr "Main.hs" >> l "."
      l "Kompilieren Sie Ihr Programm mit der"
      em "Kommandozeile" >> l ":"
    pcode Bash $ do
      c "$ ghc --make Main.hs"
      c "$ ./Main"
    p $ do
      l "Unter Windows müssen Sie" >> expr "Main.exe"
      l "aufrufen."
      

