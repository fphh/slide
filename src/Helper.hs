

module Helper where

import Data.Text (Text)

import Style
import Slide

h = header
p = pline
l = line
m = meth
c = code
q = question
a = link

question m = pline m <| [bglightblue]

para :: String -> ParagraphF String
para = pline . line

newline :: ParagraphF String
newline = pline linebreak

meth :: String -> WordF String
meth =
  let styles =
        bglightgreen
        : monospace
        : radius 99
        : paddingL 10
        : paddingR 10
        : paddingT 1
        : paddingB 1
        : []
  in (<| styles) . line

tag :: String -> WordF String
tag t =
  let styles =
        red
        : monospace
        : []
  in line ("<" ++ t ++ "></" ++ t ++ ">") <| styles

attr :: String -> WordF String
attr =
  let styles =
        blueviolet
        : monospace
        : []
  in (<| styles) . line

em :: String -> WordF String
em =
  let styles =
        fsitalic
        : fwbold
        : []
  in (<| styles) . line

header :: String -> (Text, StyleValue) -> ParagraphF String -> SlideF String
header str color = slide (line str <| [color])

d3js :: WordF String
d3js = line "d3.js" <| [orange]

webpack :: WordF String
webpack = line "webpack" <| [darkorange]


nodejs :: WordF String
nodejs = line "nodejs" <| [monospace, bgpink, blue]

jsdom :: WordF String
jsdom = line "jsdom" <| [monospace, green]

hint m = pline m <| [bgdarksalmon]


pcenter line_ = (p $ line_) <| [center]

def =
  let styles =
        fsitalic
        : blueviolet
        : []
  in (<| styles) . line

ghci = line "GHCi" <| [darkgreen, fsitalic]

cmd str = line str <| [monospace, green]

expr = cmd


chapter :: String -> String ->  (Text, StyleValue) -> SlideF String
chapter h t c = do
  header h c $ do
    newline
    (p $ l t) <| [center, fwbold, pt24]


haskell = pcode Haskell




lib :: String -> WordF String
lib =
  let styles =
        bgdarkblue
        : yellow
        : monospace
        : fsitalic
        : radius 99
        : paddingL 10
        : paddingR 10
        : paddingT 1
        : paddingB 1
        : []
  in (<| styles) . line

func :: String -> WordF String
func =
  let styles =
        maroon
        : monospace
        : fsitalic
        : fwbold
        : []
  in (<| styles) . line

