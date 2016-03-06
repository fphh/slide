

module Haskell.Intro where


import Style
import Slide
import Helper


slideShow :: SlideF String
slideShow = do
  installation
  firstSteps
  resources


installation :: SlideF String
installation = do
  header "Installation der Haskell-Plattform" red $ do
    p $ do
      l "Als Compiler benutzen wir den Glasgow Haskell Compiler (GHC)."
    p $ do
      a "https://www.haskell.org/platform/"
        "Die Haskell-Platform inklusive Compiler gibt es hier" <| [right]
      l "."
    p $ do
      l "Wer die SHA-256 Prüfsumme prüfen will, holt sich"
      a "http://www.labtestproject.com/files/win/sha256sum/sha256sum.exe"
        "sha256sum.exe"
      l "."
    p $ do
      l "Jeder beliebige Editor tut's, z.B."
      a "https://notepad-plus-plus.org/" "notepad++"
      l "."


firstSteps :: SlideF String
firstSteps = do
  header "Ihr erster Haskell-Ausdruck" red $ do
    p $ do
      l "Nach der Installation starten Sie eine cmd-Kommando-Zeile"
      l "und tippen"
    pcode Haskell $ do
      c "$ ghci"
      c "> 1+2"
      c "3"
    p $ do
      ghci >> l "verlassen Sie mit" >> cmd ":q" >> l "oder Strg-D"
    pcode Haskell $ do
      c "> :q"
      c "Leaving GHCi."
    (p $ l "Alles Ok?") <| [center]



resources :: SlideF String
resources = do
  header "Resourcen im Web" darkred $ do
    p $ do
      l "Zur Sprache:"
    pcenter $ a "https://www.haskell.org/" "Über Haskell"
    pcenter $ a "https://www.haskell.org/platform/" "Haskell-Platform"
    pcenter $ a "https://www.haskell.org/ghc/" "GHC Compiler"
    pcenter $ a "https://hackage.haskell.org/" "Software-Bibliotheken"
    pcenter $ a "https://wiki.haskell.org/Haskell" "Wiki"
    p $ do
      l "Online Bücher"
    pcenter $ a "http://learnyouahaskell.com/" "Learn You a Haskell for Great Good!"
    pcenter $ a "http://book.realworldhaskell.org/" "Real World Haskell"
