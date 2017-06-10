


module Haskell.Interpreter where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  -- interprAndComp
  ast
  helpers
  dataMap
  typeSynonyms
  interpretFunc
  test
  badError
  maybeType
  
{-
interprAndComp :: SlideF String
interprAndComp = do
  h "Interpretation und Compilation" blueviolet $ do
    p $ l "Interpretation eines Programm-Codes erfolgt in Stufen:"
    p $ l "1. Text -> Lexer -> Tokens"
    p $ l "2. Tokens -> Parser -> AST (Abstract Syntax Tree)"
    p $ l "3. AST -> Semantische Analyse -> AST"
    p $ l "4a. AST -> Interpreter -> Ergebnis"
    p $ l "bzw. alternativ für Compilation:"
    p $ l "4b. AST -> Code-Emitter -> Objekt-Code"
    p $ l "5. Objekt-Code -> Linker -> Objekt-Code"
    p $ l "6. Objekt-Code -> CPU -> Ergebnis"
    p $ l "Wir kümmern uns nur um Phase 4a."
-}

ast :: SlideF String
ast = do
  h "Abstract Syntax Tree" blueviolet $ do
    p $ do
      l "Beginnen Sie ein neues Modul" >> expr "Interpreter.hs" >> l "."
    p $ do
      l "Definieren Sie einen rekursiven Datentyp" >> expr "Expr a" >> l "."      
      l "Er soll Konstanten und Variablen, sowie unäres Minus,"
      l "Addition und Multiplikation darstellen können."
    p $ do
      l "Leiten Sie eine Instanz für die Typ-Klasse Show ab."
    p $ do
      l "Schreiben Sie eine rekursive Funktion"
    haskell $ do
      c "pretty :: (Show a) => Expr a -> String"
    p $ do
      l "die für eine hübsche Ausgabe sorgt."      
      l "Beginnen Sie mit den Basisfällen."
 
helpers :: SlideF String
helpers = do
  h "Erleichterungen" blueviolet $ do
    p $ do
      l "Mittels" >> expr "Expr a" >> l "definieren Sie"
      l "Variablen" >> expr "x, y, z"
      l "Konstanten" >> expr "_1, _2, _3"
      l "und die Operatoren"
      expr "(.+)" >> l ","
      expr "(.*)" >> l "und"
      expr "(.-)" >> l "."
    p $ do
      l "Mit" >> expr ":info"
      l "können Sie die Bindungsstärke und Assoziativität"
      l "der build-In-Operatoren"
      expr "(+)" >> l "etc."
      l "ermitteln."
    p $ do
      l "Definieren Sie diese für Ihre Operatoren entsprechend."
    p $ do
      l "Jetzt sollten Sie im" >> ghci >> l "eingeben können:"
    haskell $ do      
      c "> x .* _1 + _2"
      c "> y .* (x .+ _7)"


dataMap :: SlideF String
dataMap = do
  h "Bibliothek Data.Map" blueviolet $ do
    p $ do
      l "Wir brauchen eine Datenstruktur, um den Variablen"
      l "bei der Interpretation eine Wert zu geben:"
    p $ do
      a "https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html"
        "Machen Sie sich mit dem Modul Data.Map vertraut."
    p $ do
      l "Speziell mit den Funktionen"
      lib "singleton" >> l ","
      lib "fromList" >> l "und"
      lib "lookup" >> l "."
      l "Importieren Sie" >> expr "Data.Map" >> l "als"
    haskell $ do     
      c "import qualified Data.Map as Map"
      c "import Data.Map (Map)"
    p $ do
      l "Laden Sie Ihr Modul und testen Sie die Funktionen im"
      ghci >> l "."

typeSynonyms :: SlideF String
typeSynonyms = do
  h "Typsynonyme" blueviolet $ do
    p $ do
      l "Mit" >> expr "type"
      l "können Sie Typsynonyme definieren."
      l "So ist z.B."
      expr "String" >> l "definiert als:"
    haskell $ do
      c "type String = [Char]"
    p $ do      
      l "Definieren Sie mit Hilfe von"
      expr "Data.Map"
      l "einen Typsynonym" >> expr "Env a"
      l "für"
      l "das Interpretations-Environment."
      l "Der Schlüssel bei"
      expr "Map k a"
      l "entspricht dem Typen der Variablennamen."
    p $ do
      l "Definieren Sie mit"
      lib "fromList"
      l "ein Environment für Ihre Variablen, d.h."
      l "eine Zuordnung von Namen zu Werten."


interpretFunc :: SlideF String
interpretFunc = do
  h "Und jetzt der Interpreter" blueviolet $ do
    p $ do
      l "Definieren Sie eine rekursive Funktion"
    haskell $ do
      c "interpret ::"
      c "    (Num a) => Env a -> Expr a -> a"
    p $ do      
      l "Beginnen Sie mit den Basisfällen."
    p $ do
      l "Um Variablen zu interpretieren, benutzen Sie"
      func "lookup"
      l "und unterscheiden die Fälle mit" >> expr "case" >> l "."
      l "Im" >> expr "Nothing"
      l "-Fall benutzen Sie" >> warn "ausnahmsweise"
      l "die Funktion"
      lib "error" >> l "."

test :: SlideF String
test = do
  h "Ausprobieren" blueviolet $ do
    p $ do
      l "Testen Sie Ihre Funktion:"
    haskell $ do           
      c "> let env = Map.fromList [(\"x\", 7), (\"y\", 10)]"
      c "> let expr = x .+ y .* _3"
      c "> interpret env expr"
      c "37"


badError :: SlideF String
badError = do
  h "Schlimmer Fehler" red $ do
    p $ do
      l "Unserer Implementierung"
      l "passt nicht zum Typen der Funktion"
    haskell $ do
      c "interpret ::"
      c "    (Num a) => Env a -> Expr a -> a"
    question $ do
      l "Wer sieht, warum?"
    p $ do
      l "Schreiben Sie einen Ausdruck, der das"
      l "Problem aufzeigt."
    question $ do
      l "Verbesserungsvorschläge?"


maybeType :: SlideF String
maybeType = do
  h "Maybe a" green $ do
    p $ do
      l "Ein wichtiger Daten-Typ ist" >> expr "Maybe a"
      l "."
    haskell $ do
      c "data Maybe a = Nothing | Just a"
    p $ do
      l "Der Typkonstruktor" >> expr "Maybe"
      l "nimmt als Argument einen Typen,"
      l "repräsentiert durch die Typvariable" >> expr "a" >> l ","
      l "um einen Typen zu bilden."
    p $ do
      l "Lassen Sie sich mit" >> cmd ":info"
      l "Informationen zu" >> expr "Maybe"
      l "ausgeben."
    question $ do
      l "Wie würden Sie den Typen" >> expr "Maybe a"
      l "interpretieren?"
    p $ do
      l "Definieren Sie" >> expr "interpret"
      l "um, so dass ein" >> expr "Maybe a"
      l "zurückgegeben wird."
