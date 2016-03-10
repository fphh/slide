
module Haskell.FunctorAndApplicative where


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  functor
  applicative
  liftFunc
  badErrorCorrection
  
functor :: SlideF String
functor = do
  h "Typklasse Functor" magenta $ do
    p $ do
      l "Die Typklasse"
      expr "Functor"
      l "beinhaltet Typen der Form"
      expr "f a"
      l ", wobei" >> expr "t"
      l "ein Typkonstruktor ist,"
      l "der ein Typargument benötigt."
    p $ do
      l "Die Typklasse ist so definiert."
    haskell $ do
      c "class Functor f where"
      c "    fmap :: (a -> b) -> f a -> f b"
    p $ do
      l "Beispiele:"
      expr "[a], Maybe a, Either b a, IO a, ..."
    p $ do
      l "Auf all diese Typen können Sie"
      lib "fmap" >> l "anwenden."
    p $ do
      l "Testen Sie mit" >> expr "[a]"
      l "und" >> expr "Maybe a"
      l "."
      a "https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Functor.html"
        "Dokumentation"

applicative :: SlideF String
applicative = do
  h "Typklasse Applicative" magenta $ do
    p $ do
      l "Die Typklasse"
      expr "Applicative"
      l "beinhaltet ebenfalls Typen der Form"
      expr "f a"
      l "."
    haskell $ do
      c "class Functor f => Applicative f where"
      c "      pure :: a -> f a"
      c "      (<*>) :: f (a -> b) -> f a -> f b"
    p $ do
      l "Alle Typen von" >> expr "Applicative"
      l "sind also auch Mitglieder von"
      expr "Functor" >> l "."
    p $ do
      l "Beispiele:"
      expr "[a], Maybe a, Either b a, IO a, ..."
    p $ do
      l "Machen Sie sich mit der"
      a "https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Applicative.html"
        "Dokumentation"
      l "vertraut."

liftFunc :: SlideF String
liftFunc = do
  h "Die Funktion liftA2" magenta $ do
    p $ do
      l "Insbesondere interessant ist auch diese Funktion:"
    haskell $ do
      c "liftA2 :: Applicative f =>"
      c "   (a -> b -> c) -> f a -> f b -> f c"
    p $ do
      l "So zum Beispiel:"
    haskell $ do
      c "> liftA2 (+) (Just 1) (Just 2)"
      c "Just 3"
    haskell $ do
      c "> liftA2 (+) (Just 1) Nothing"
      c "Nothing"     



badErrorCorrection :: SlideF String
badErrorCorrection = do
  h "Fehlerbehandlung mit Maybe" red $ do
    p $ do
      l "Beheben Sie den Fehler in Ihrem Interpreter."
      l "Dazu müssen Sie sowohl dessen Typen anpassen,"
      l "als auch die Implementierung."
    p $ do
      l "Die Funktion" >> func "interpret"
      l "soll nunmehr ein" >> expr "Maybe a"
      l "zurückliefern."
    p $ do
      l "Ändern Sie zunächst den Typen."
    p $ do
      l "Kommentieren Sie die rekursiven Fälle aus"
      l "und passen Sie zunächst die Basisfälle an."
    question $ do
      l "Was sollen Konstanten und Variablen zurückliefern?"
    p $ do
      l "Für die rekursiven Fälle benutzen Sie"
      lib "fmap" >> l "und"
      lib "liftA2"
      l "."
