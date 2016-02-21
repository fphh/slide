

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
  dataJoin
  selection
  enterSelection
  enterSelectionExercice
  introSVG
  circleElement
  exerciseCircle
  tooltipExercise
  selectionExercise
  
--  groupElement
  
titlePage :: SlideF String
titlePage = do
  slide (word "") $ do
    para "Eine Einführung in" <| [pt24, center]
    pimage "html/d3_logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    newline
    para "März 2016"  <| [center]


directory :: SlideF String
directory = do
  h "Verzeichnisstruktur" red $ do
    p $ do
      l "Downloaden Sie"
      a "html/myD3.zip" "myD3.zip"
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
      l "wählen Sie einen Knoten im DOM."
      l "Mittels Method Chaining können"
      l "ein neuen Knoten einfügen und seinen Text setzen."
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
    p $ do
      l "Wenn Sie" >> tag "ol" >> l "gewählt haben"
      l "können Sie z.B. das Attribut" >> attr "start" >> l "setzen."

dataJoin :: SlideF String
dataJoin = do
  h "Data Joins" deepskyblue $ do
    p $ do
      l "Das grundlegende Konzept von" >> d3js >> l "sind Data Joins."
      l "Data Joins verbinden Elemente des DOM's mit Daten."
    p $ do
      l "Zuerst wählt man die Elemente mit der Methode" >> m "selectAll"
      l "aus, dann verbindet man die Elemente mit einem Array von Daten"
      l "mittels der Methode" >> m "data" >> l "."
    pcode JavaScript $ do
      c "var selection = d3.select('svg')"
      c "    .selectAll('li')"
      c "    .data(['A', 'B', 'C'])"
      


selection :: SlideF String
selection = do
  h "Selektionen" deepskyblue $ do
    p $ do
      l "Auf der mit" >> m "selectAll" >> l "geschaffenen Selektion"
      l "wird nun weitergearbeitet. Drei Fälle sind zu unterscheiden:"
    p $ l "1. Das Datum ist neu im Datensatz:" >> em "enter" <| [blue]
    p $ l "2. Das Datum war schon da:" >> em "update" <| [darksalmon]
    p $ l "3. Das Datum ist nicht mehr vorhanden im Datensatz:" >> em "exit" <| [orange]
    pimage "html/updateEnterExit.svg" <| [center]
    p (l "Zunächst kümmern wir uns nur um neue Elemente.")


enterSelection :: SlideF String
enterSelection = do
  h "Die enter-Selektion" deepskyblue $ do
    p $ do
      l "In der enter-Selektion sind nur neue Element."
      l "Zu Beginn sind das also alle Elemente."
      l "Auf der enter-Selektion arbeitet man, indem man" >> m "enter"
      l "aufruft."
    pcode JavaScript $ do
      c $ "selection.enter()"
      c $ "    .append('li')"
      c $ "    .text(function (d, idx) { return d; })"
    p $ do
      l "Überall, wo Sie Text, Attribute oder Styles in Abhängigkeit"
      l "der Daten angeben wollen, können Sie eine Funktion einsetzen."
      l "Der erste Parameter ist das Datum, der zweite der Index im Array."


enterSelectionExercice :: SlideF String
enterSelectionExercice = do
  h "Übung: enter-Selektion" deepskyblue $ do
    p $ do
      l "Definieren Sie ein Array mit Strings (z.B. Namen)."
      l "Bauen Sie mit diesen Namen eine Liste."
      l "Verwenden Sie Data Joins und die enter-Selektion."
    p $ do
      l "Geben sie den" >> (l "geraden" <| [red])
      l "Punkten eine andere Textfarbe als"
      l "ungeraden" <| [green] >> l "."
    p $ do
      l "Erweitern Sie die Daten im Array so, dass"
      l "zu jedem Namen ein Alter assoziert ist."
      l "Färben sie die Namen derer, die unter 18 sind"
      l "mit einer anderen Farbe als die Namen derer, die über 18 sind."


  
introSVG :: SlideF String
introSVG = do
  h "Einführung zu SVG" crimson $ do
    p $ do
      l "SVG ist eine Spezifikation zur Beschreibung von Vektor-graphiken."
      l "Es basiert auf XML und lässt sich gut in HTML-Seiten einbetten."
    p $ do
      l "SVG-Graphiken werden immer mit" >> tag "svg" >> l " umschlossen."
      l "Als Attribute werden" >> attr "width"
      l "und" >> attr "height" >> l "benötigt."
    p $ do
      l "Um SVG-Graphiken stand-alone im Browser betrachten zu können,"
      l "muss man noch das Attribut" >> attr "xmlns" >> l "setzen."
    pcode HTML $ do
      c "<svg width='800' height='600'>"
      c "    ..."
      c "</svg>"




circleElement :: SlideF String
circleElement = do
  h "Kreise" crimson $ do
    p $ do
      l "Kreise werden in SVG mit dem Element" >> tag "circle"
      l "erzeugt. Sie erhalten die Attribute" >> attr "cx"
      l "und" >> attr "cy" >> l "um den Mittelpunkt anzugeben,"
      l "sowie" >> attr "r" >> l "für den Radius."
      l "Natürlich können sie auch" >> attr "class"
      l ", " >> attr "id" >> l "oder" >> attr "style" >> l "benützen."



exerciseCircle :: SlideF String
exerciseCircle = do
  h "Übung: Kreise" crimson $ do
    p $ do
      l "In der Übung"
      a "html/myD3Solutions/example1/index_v1.html" "Kreise"
      l "erzeugen Sie ein Array mit zufälligen Radien."
    p $ do
      l "Statt Schleifen sollen Sie die Funktionen"
      m "d3.range" >> l "," >> m "map" >> l ","
      m "sort" >> l "und" >> m "d3.ascending" >> l "benützen."
    p $ do
      l "Machen Sie sich mit den"
      a "https://github.com/mbostock/d3/wiki/Arrays" "Array-Funktionen"
      l "vertraut."
    p $ l "Verwenden Sie eine Skala von" >> d3js >> l ":"
    pcode JavaScript $ do
      c "> var color = d3.scale.category10()"
      c "      .domain([0, 9])"
      c "> color(4)"
      c "> \"#9467bd\""


groupElement :: SlideF String
groupElement = do
  h "Einführung zu SVG" crimson $ do
    p $ do
      l "Groupenelement Transfrom"



tooltipExercise :: SlideF String
tooltipExercise = do
  h "Übung: Tooltip" pink $ do
    p $ do
      a "html/myD3Solutions/example1/index_v1.html" "Aufgabe 1"
      a "html/myD3Solutions/example1/index_v2.html" "Aufgabe 2"
      a "html/myD3Solutions/example1/index_v3.html" "Aufgabe 3"


selectionExercise :: SlideF String
selectionExercise = do
  h "Übung: Selektionen" pink $ do
    p $ do
      l "Für folgende Übung brauchen Sie alles,"
      l "was Sie über Selektionen gelernt haben:"
    p $ a "html/myD3Solutions/selection/index.html" "Übung zu Selektionen"
    p $ l "Überdies sollten Sie sich mit dem Gruppen-Element auskennen."


      

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
        cursive
        : fsitalic
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
