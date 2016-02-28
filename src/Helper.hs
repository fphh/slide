

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

nodejs :: WordF String
nodejs = line "nodejs" <| [monospace, bgpink, blue]

jsdom :: WordF String
jsdom = line "jsdom" <| [monospace, green]

hint m = pline m <| [bgdarksalmon]
