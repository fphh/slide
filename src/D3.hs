

module D3 where

import Style
import Slide

import Data.Text (Text)

slideShow :: SlideF String
slideShow = do
  titlePage
  directory
  introD3
  introDOM
  select
  selectEx
  styleAndAttr

  
titlePage :: SlideF String
titlePage = do
  slide (word "") $ do
    para "Eine Einführung in" <| [pt24, center]
    pimage "d3_logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    newline
    para "März 2016"  <| [center]


directory :: SlideF String
directory = do
  h "Verzeichnisstruktur" red $ do
    p $ do
      l "Downloaden Sie"
      a "myD3.zip" "myD3.zip"
      l "und entpacken Sie es in Ihrem Arbeitsverzeichnis."
      l "Folgende Struktur sollten Sie vorfinden:"
    pcode Directory $ do
      c "myD3/"
      c "    d3/"
      c "        d3.min.js"
      c "    example1/"
      c "        index.html"
      c "        script.js"
      c "        format.css"
      c "..."
    p $ do
      l "Alternative können Sie"
      a "https://github.com/mbostock/d3/releases/download/v3.5.12/d3.zip" "d3.min.js"
      l "auch selbst herunterladen."

introD3 :: SlideF String
introD3 = do
  h "D3.js Überblick" red $ do
    p $ do
      l "" >> d3js
      l "ist eine beliebte JavaScript-Bibliothek,"
      l "um SVG-Graphiken zu erzeugen."
    p $ do
      l "Prinzipiell kann aber eine beliebige XML-artige"
      l "Baum-Struktur erzeugt werden."

introDOM :: SlideF String
introDOM = do
  h "Ein spezielles DOM-Element" red $ do
    p $ do
      l "Damit" >> d3js
      l "funktioniert, muss ein Element"
      l "im DOM identifiziert werden."
    p $ do
      l "Das DOM wird üblicherweise von einem Browser bereitgestellt."
    p $ do
      l "In unseren Beispielen soll das Element,"
      l "in dem die SVG-Graphik erzeugt wird, folgendermaßen aussehen:" 
    pcode HTML $ do
      c "<div id=\"app\"></div>"

select :: SlideF String
select = do
  h "d3.select, text & append" orange $ do
    p $ do
      l "Mit der Funktion" >> meth "d3.select"
      l "wählen Sie ein Element aus dem DOM."
      l "Mittels Method Chaining können Sie dem gewählten Element"
      l "ein neues Element anhängen und dessen Text setzen."
    pcode JavaScript $ do
      c "d3.select(\"#app\")"
      c "    .append(\"hello\")"
      c "    .text(\"Guten Tag!\");"
    para "Als Ergebnis sollten Sie folgendes sehen:"
    pcode HTML $ do
      c "<div id=\"app\">"
      c "    <hello>Guten Tag!</hello>"
      c "</div>"

selectEx :: SlideF String
selectEx = do
  h "d3.select & append Übung" orange $ do
    p $ do
      l "Legen Sie ein neues Projekt an und bringen Sie"
      l "das vorherige Beispiel zum Laufen."
    p $ do
      l "Erzeugen Sie eine Liste mit drei Elementen."
      l "Speichern sie das Listen-Element in einer Variable zwischen,"
      l "bevor sie die drei Elemente mit append einfügen."
    question $ do
      l "Was passiert, wenn Sie mehrere appends mittels Method"
      l "Chaining auf dem selben Element ausführen?"
      
styleAndAttr :: SlideF String
styleAndAttr = do
  h "style & attr" orange $ do
    p $ do
      l "Mit der Chained Method" >> m "style"
      l "können Sie ein Listen-Element individuell stylen."
      l "Ändern Sie z.B. die Textfarbe eines Elements."
    p $ do
      l "Mit der Chained Method" >> m "attr"
      l "können Sie den Elementen Attribute geben."
      l "Geben Sie einem Ihrer Listen-Elemente"
      l "z.B. ein class-Attribut und stylen Sie das Element darüber."



-- helper


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

header :: String -> (Text, StyleValue) -> ParagraphF String -> SlideF String
header str color = slide (line str <| [color])


d3js :: WordF String
d3js = line "d3.js" <| [orange]

nodejs :: WordF String
nodejs = line "nodejs" <| [monospace, bgpink, blue]

jsdom :: WordF String
jsdom = line "jsdom" <| [monospace, green]

                   
{-
select :: SlideF String
select = do
  slide (line "Selektieren von DOM-Elementen" <| [red]) $ do
    pline $ do
      line "Nach dem laden von"
      line "d3.js" <| [orange]
      -}
      
-- https://en.wikipedia.org/wiki/Information_design
{-
var xs = d3.range(1, 10).map(function () { return Math.round(100*Math.random()); })
undefined
d3.select("#app").selectAll("p").data(xs).enter().append("p").text(function (d) { return d + " "; })
-}
