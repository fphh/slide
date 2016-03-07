

module D3.D3 where

import Style
import Slide
import Helper

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
  textElement
  rectElement
  groupElement
  mouseEvents
  tooltipExercise
  selectionII
  selectionExit
  selectionUpdate
  selectionEnterAndUpdate
  selectionDataIndex
  transitions
  selectionExercise
  theEnd
  
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
      c "    .append(\"p\")"
      c "    .text(\"Guten Tag!\");"
    para "Als Ergebnis sollten Sie folgendes sehen:"
    pcode HTML $ do
      c "<div id=\"app\">"
      c "    <p>Guten Tag!</p>"
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
      c "var selection = d3.select('#app')"
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
  h "Einführung in SVG" crimson $ do
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
    p $ do
      l "Einen Beschreibung aller SVG-Elementen finden Sie"
      a "https://developer.mozilla.org/en-US/docs/Web/SVG" "hier"
      l "."

exerciseCircle :: SlideF String
exerciseCircle = do
  h "Übung: Kreise" crimson $ do
    p $ do
      l "In der Übung"
      a "html/myD3Solutions/example1/index_v1.html" "Kreise"
      l "erzeugen Sie ein Array mit zufälligen Radien, aufsteigen sortiert."
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

textElement :: SlideF String
textElement = do 
  h "Text" crimson $ do
    p $ l "Das Element" >> tag "text" >> l "erlaubt es, Texte einzufügen:"
    pcode JavaScript $ do
      c "d3.select('svg')"
      c "    .append('text')"
      c "    .text('hello')"
    p $ do
      l "Die Possition des Textes wird mit den Attributen" >> attr "x"
      l "und" >> attr "y" >> l "bestimmt."
      l "Achten Sie darauf, dass dies die linke untere Ecke des Texts"
      l "angibt."
    p $ do
      l "Den Text selbst fügen Sie mit" >> m "text" >> l "ein."
      l "Auch hier können Sie stattdessen eine Funktion einsetzen,"
      l "um auf die zugeordneten Daten zuzugreifen."
      

rectElement :: SlideF String
rectElement = do 
  h "Rechtecke" crimson $ do
    p $ do
      l "Das Element" >> tag "rect" >> l "fügt Rechtecke ein."
      l "Die prinzipiellen Attribute von Rechtecken sind" >> attr "x"
      l "," >> attr "y" >> l "," >> attr "width" >> l "und" >> attr "height"
      l "."
    p $ do
      l "Achten Sie darauf, dass" >> attr "x"
      l "und" >> attr "y" >> l "die linke obere Ecke des"
      l "Rechtecks beschreiben, während" >> attr "width"
      l "und" >> attr "height"
      l "dessen Ausdehnung nach rechts unten angeben."



groupElement :: SlideF String
groupElement = do
  h "Das Gruppenelement" crimson $ do
    p $ do
      l "Das SVG-Gruppenelement" >> tag "g"
      l "erlaubt es, SVG-Element zu gruppieren:"
    pcode JavaScript $ do
      c "var group = d3.select('svg').append('g');"
      c "group.append('rect');"
      c "group.append('text');"
    p $ do
      l "Das wichtigste Attribute ist" >> attr "transform" >> l "."
      l "Um das Gruppenelement samt Inhalt entlang der x/y-Achsen"
      l "zu verschieben, setzen Sie es auf" >> em "translate(x, y)" >> l ","
      l "wobei" >> em "x" >> l "und" >> em "y"
      l "die Zahl der Pixel angibt."
    p $ do
      l "Andere Transformationen sind " >> em "scale"
      l "und" >> em "rotate" >> l "."

mouseEvents :: SlideF String
mouseEvents = do
  h "Mouse Events" crimson $ do
    p $ do
      l "Mit der Methode" >> m "on" >> l "fügen Sie Ihren Elementen"
      l "Mouse-Event-Listener hinzu."
    pcode JavaScript $ do
      c "svg.selectAll('circle')"
      c "    .on('click', function (d, idx) { ... });"
    p $ do
      l "Wichtige Mouse-Events sind auch" >> em "mouseenter,"
      em "mousemove," >> em "mouseleave" >> l "und" >> em "mouseout."
      l "Hier finden Sie einen Überblick über"
      a "https://developer.mozilla.org/en-US/docs/Web/Events#Standard_events" "Mouse Events"
      l "."
    p $ do
      l "Im Callback bekommen Sie mit" >> m "d3.select(this)"
      l "einen Zeiger auf das Mouse-Event zurück."

tooltipExercise :: SlideF String
tooltipExercise = do
  h "Übung: Tooltip" crimson $ do
    p $ do
      l "Im Beispiel"
      a "html/myD3Solutions/example1/index_v2.html" "Tooltip"
      l "erweitern Sie Ihre Kreise um Tooltips."
    p $ do
      l "Um die Maße des Tooltips zu berechnen, setzen Sie"
      l "im Callback zuerst dessen Text neu,"
      l "dann rufen Sie auf dem Text-Konten die Methode " >> m "getBBox"
      l "auf."
    pcode JavaScript $ do
      c "text.text('neuer text');"
      c "var bbox = text.node().getBBox();"
      
    p $ do
      l "Je nach Zeit und Laune können Sie zur Übung diese"
      a "html/myD3Solutions/example1/index_v3.html" "Aufgabe"
      l "bearbeiten."


selectionII :: SlideF String
selectionII = do
  h "Nochmal Selektionen" green $ do
    p $ do
      l "Die Selektionen entstehen beim Aufrufen von" >> m "data"
    pcode JavaScript $ do
      c "var selection = svg"
      c "    .selectAll('circle')"
      c "    .data(newData);"
    p $ do
      l "Jedes Mal, wenn man neue Daten darstellen will,"
      l "muss man also" >> m "data" >> l "aufrufen."
      l "Drei Selektionen gibt es:" >> em "enter," <| [blue]
      em "update" <| [darksalmon] >> l "und" >> em "exit" <| [orange]
      l "."
    pimage "html/updateEnterExit.svg" <| [center]

selectionExit :: SlideF String
selectionExit = do
  h "Die Exit-Selektion" green $ do
    p $ do
      l "Die Exit-Selektion dient üblicherweise dazu,"
      l "Elemente zu entfernen, indem man" >> m "remove" >> l "aufruft."
    pcode JavaScript $ do
      c "selection"
      c "    .exit()"
      c "    .remove();"

selectionUpdate :: SlideF String
selectionUpdate = do
  h "Die Update-Selektion" green $ do
    p $ do
      l "Die Update-Selektion dient dazu, die Attribute und Styles der"
      l "verbliebenen Elemente neu zu setzen:"
    pcode JavaScript $ do
      c "selection"
      c "    .attr('cx', function (d, idx) { ... });"
      c "    .attr('cy', function (d, idx) { ... });"
      c "    .style('fill', 'yellow');"
    p $ do
      l "Hierzu muss man nicht erst eine spezielle Methode wie"
      m "enter" >> l "oder" >> m "exit" >> l "aufrufen."

selectionEnterAndUpdate :: SlideF String
selectionEnterAndUpdate = do
  h "Enter und Update" green $ do
    p $ do
      l "Sobald man die enter-Selektion bearbeitet hat"
      l "vereinen sich update- und enter-Selektion."
    pcode JavaScript $ do
      c "var selection = svg"
      c "    .selectAll('circle')"
      c "    .data(newData);"
      c "selection.enter().append('circle');"
      c "selection.attr('r', 99);"
    p $ do
      l "Im Beispiel bekommen alle Kreise den Radius 99,"
      l "sowohl die neuen als auch die schon vorhandenen."
    p $ do
      l "Die Reihenfolge, in der man die Selektionen bearbeitet,"
      l "ist sehr wichtig."

selectionDataIndex :: SlideF String
selectionDataIndex = do
  h "Identifizieren von Elementen" green $ do
    p $ do
      l "Der Methode" >> m "data" >> l "übergibt man zunächst ein Array."
      l "Die Array-Indices identifizieren die Elemente,"
      l "während die Selektionen berechnet werden."
      l "Zusätzlich gibt es die Möglichkeit, eine Key-Funktion anzugeben:"
    pcode JavaScript $ do
      c "function identity(x) { return x; }"
      c "var letters = 'abcdefgh'.split('');"
      c "var selection = svg"
      c "    .selectAll('circle')"
      c "    .data(letters, identity);"
    p $ do
      l "Im Beispiel werden die Daten jetzt über ihren Inhalt,"
      l "nämlich den Buchstaben, identifiziert."


transitions :: SlideF String
transitions = do
  h "Transitionen" blue $ do
    p $ do
      l "Mit der chained Method" >> m "transition"
      l "leitet man einen sachten Übergang von altem auf neuen Wert ein."
      l "Mit" >> m "duration" >> l "gibt man dessen Dauer an."
    pcode JavaScript $ do
      c "cirlces.attr('r', 40)"
      c "   .transtition()"
      c "   .duration(1000)"
      c "   .attr('cx', 99)"
      c "   .style('fill', 'blue');"
    p $ do
      l "Bei Transtionen auf dem selben Element unterbricht"
      l "die jungere die ältere."
      l "Die ältere Transition bleibt möglicherweise unvollständig."


selectionExercise :: SlideF String
selectionExercise = do
  h "Übung: Selektionen" blue $ do
    p $ do
      l "In der folgenden"
      a "html/myD3Solutions/selection/index.html" "Übung"
      l "können Sie den Umgang mit Selektionen und Transitionen üben."
    p $ do
      l "Mehr über den Umgang mit Selektionen finden Sie hier:"
    pline $ a "https://bl.ocks.org/mbostock/3808218" "General Update Pattern I"
    pline $ a "https://bl.ocks.org/mbostock/3808221" "General Update Pattern II"
    pline $ a "https://bl.ocks.org/mbostock/3808234" "General Update Pattern III"


theEnd :: SlideF String
theEnd = do
  h "Vielen Dank" red $ do
    pimage "html/d3_logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    para "Not the End!" <| [center, fwbold, pt24]
    
