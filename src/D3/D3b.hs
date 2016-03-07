

module D3.D3b where

{-

* Aufbau großer Graphiken in Ebenen (z-order)
* svg und inner mit margin

* viewbox-Attribut
* canvas in IE


* Referenzieren von urls im chart überall aus DOM möglich


<image x="20" y="10" width="320" height="240" xlink:href="raupen.jpg"/>
* webpack/uglify
* jsdom/serverside/nodejs
* es2015 import/export
* selectionen in variablen speichern statt nochmal select aufrufen
* Pfad-Element Punkte pushen

* Javascript


-}


import Style
import Slide
import Helper

slideShow :: SlideF String
slideShow = do
  titlePage
  linearScales
  axes
  callFunction
  recap
  updateAxis
  ordinalScale
  layers
  barChartExample
  chainedTransitions
  chainedTransitionsII
  chainedTransitionsIII
  chainedTransitionsIV
  excercises
  pathElement
  lineFunction
  clipPaths
  timeScale
  files
  csvFiles
  crossBrowserOrigin
  yahooAPI
  colorScales
  viewbox
  webpackSlide
  theEnd


titlePage :: SlideF String
titlePage = do
  slide (word "") $ do
    para "Eine Einführung in" <| [pt24, center]
    pimage "html/d3_logo.svg" <| [center]
    pline (l "Teil II") <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    para "März 2016"  <| [center]


linearScales :: SlideF String
linearScales = do
  h "Lineare Skalen" seagreen $ do
    p $ do
      l "Skalen bilden eine Definitionsmenge auf eine Zielmenge ab."
    pcode JavaScript $ do
      c "var yscale = d3.scale.linear()"
      c "    .domain([0, 10])"
      c "    .range([100, 1000]);"
    p $ do
      l "Hier ist das Interval" >> em "[0, 10]" >> l "die Definitionsmenge"
      l "und das Interval" >> em "[100, 1000]" >> l "die Zielmenge."
    q $ do
      l "Welchen Wert ergibt" >> em "yscale(5)" >> l "?"
    p $ do
      l "Bei y-Achsen müssen Sie die Zielmenge eventuell umgekehrt angeben."


axes :: SlideF String
axes = do
  h "Achsen" seagreen $ do
    p $ do
      l "Mit Hilfe einer Skala kann man eine Achse definieren:"
    pcode JavaScript $ do
      c "var yAxis = d3.svg.axis()"
      c "    .scale(yscale)"
      c "    .orient('left');"
    p $ do
      l "Die Orientierung gibt die Ausrichtung der ticks an."
      l "Ausser" >> em "left" >> l "gibt es noch"
      em "right, top" >> l "und" >> em "bottom."

callFunction :: SlideF String
callFunction = do
  h "Die Funktion call" seagreen $ do
    p $ do
      em "yAxis" >> l "ist eine Funktion, der man eine Selektion"
      l "übergeben kann."
    pcode JavaScript $ do
      c "var sel = d3.select('svg')"
      c "    .append('g')"
      c "    .attr('class', 'y axis');"
    p $ do
      l "Zwei Möglichkeiten hat man:"
    pcode JavaScript $ do
      c "yAxis(sel)        /*  1. */"
      c "sel.call(yAxis)   /*  2. */"
    p $ do
      l "Beide Varianten bewirken, dass"
      l "eine Achse gezeichnet wird."

updateAxis :: SlideF String
updateAxis = do
  h "Achsen updaten" deepskyblue $ do
    p $ do
      l "Beim updaten von Achsen ändert sich normalerweise"
      l "der Definitionsbereich:"
    pcode JavaScript $ do
      c "xscale.domain(newDomain);"
    p $ do
      l "Dannach ruft man nocheinmal"
      em "yAxis" >> l "auf der Selektion auf."
    pcode JavaScript $ do
      c "d3.select('.y.axis')"
      c "    .transition()"
      c "    .duration(1000)"
      c "    .call(yAxis);"
    p $ do
      l "Dabei kann man natürlich Transitionen verwenden."


recap :: SlideF String
recap = do
  h "Skalen Zusammengefasst" seagreen $ do
    p $ l "Zur Wiederholung:"
    p $ l "1. Man definiert eine Skala mit" >> em "range" >> l "und" >> em "domain."
    p $ l "2. Man definiert eine Achse mit Hilfe der Skala."
    p $ do
      l "3. Man ruft" >> em "call" >> l "mit der Achse auf"
      l "oder übergibt der Achse eine Selektion."
    hint $ do
      l "Falls Sie die Achse nicht sehen, prüfen Sie im DOM"
      l "wo die Achse sich befindet und dass sie so gestylt ist,"
      l "dass man sie sieht."

ordinalScale :: SlideF String
ordinalScale = do
  h "Ordinale Skalen" orange $ do
    p $ do
      l "Ordinale Skalen haben eine diskrete Definitionsmenge."
    pcode JavaScript $ do
      c "var x = d3.scale.ordinal()"
      c "    .domain([0, 1, 2])"
      c "    .rangeRoundBands(rangeInterval, padding);"
    pimage "https://f.cloud.github.com/assets/230541/538688/46c298c0-c193-11e2-9a7e-15d9abcfab9b.png" <| [center]
    p $ do
      l "Bild und Beschreibung siehe"
      a "https://github.com/mbostock/d3/wiki/Ordinal-Scales#ordinal_rangeBands" "hier"
      l "."

layers :: SlideF String
layers = do
  h "Aufbau komplexer Graphiken" green $ do
    p $ do
      l "Für große Graphiken sollten Sie"
      l "mehrere Layers in Form von Gruppenelementen"
      l "anzulegen und diese mit" >> em "transform"
      l "zu positionieren."
    pcode HTML $ do
      c "<svg>"
      c "    <defs> ... </defs>"
      c "    <g id='chart'> ... </g>"
      c "    <g id='tooltip'> ... </g>"
      c "    ..."
      c "</svg>"
    p $ do
      l "Die Layer sorgen dafür, dass die Elemente"
      l "auf der richtigen Ebene im" >> em "z-index"
      l "landen."

barChartExample :: SlideF String
barChartExample = do
  h "Bar Chart" orange $ do
    p $ do
      l "Bearbeiten Sie diese"
      a "html/myD3Solutions/example2/index_v1.html" "Aufgabe."
    hint $ do
      l "Benutzen Sie die x- und y-Skalen, um die x-Position bzw."
      l "Höhe Ihrer Rechtecke zu berechnen."
    hint $ do
      em "x.rangeBands()" >> l "liefert die Breite der Rechtecke zurück."
    p $ do
      l "Im Anschluss erweitern Sie Ihre Lösung"
      a "html/myD3Solutions/example2/index_v2.html" "folgendermaßen."

chainedTransitions :: SlideF String
chainedTransitions = do
  h "Verkettete Transitionen" lightblue $ do
    p $ do
      l "Für Animationen verwenden wir Transitionen."
      l "Manchmal möchte man die Animation kaskadieren."
    p $ do
      l "Ein assynchrones" >> em "setTimeout()"
      l "zu verwenden, ist keine gute Idee."
      l "Triggert der Timeout zu früh, bleibt"
      l "die vorausgehende Transition unvollendet."
    hint $ do
      l "Manche Situationen kann man so auch gar nicht darstellen."
      l "Sobald man z.B. eine Transition auf" >> em "enter+update"
      l "anwendet, werden die Transitionen für" >> em "enter"
      l "und" >> em "update" >> l "unterbrochen."

chainedTransitionsII :: SlideF String
chainedTransitionsII = do
  h "Transitionen aus dem Nichts" lightblue $ do
    p $ do
      l "Eine Transition kann auch ohne Selektion"
      l "erzeugt werden. Im Callback von" >> em "each()"
      l "kann man die Selektionen bearbeiten."
    pcode JavaScript $ do
      c "var t0 = d3.transition().duration(500).each(cb);"
      c "function cb() {"
      c "    circles"
      c "        .attr('cx', 20)"
      c "        .transition()"
      c "        .attr('cy', 100);"
      c "}"
    p $ do
      l "Der Aufruf von" >> em "transtion()"
      l "löst die Transition für die nachfolgenden Attribute und Styles aus."


chainedTransitionsIII :: SlideF String
chainedTransitionsIII = do
  h "Transitionen aus anderen Transitionen" lightblue $ do
    p $ do
      l "Eine Transition kann man auch aus einer anderen"
      l "Transition ableiten."
    pcode JavaScript $ do
      c "var t1 = t0.transition()"
      c "    .duration(700).each(cb2);"
    p $ do
      l "Die in" >> em "cb2" >> l "ausgelösten Transitionen"
      l "kommen erst zum Zug, wenn die vom Callback in"
      em "t0" >> l "ausgelösten beendet sind."
    p $ do
      l "Die Transitionen sind also synchron."
    p $ do
      l "Von" >> em "t1" >> l "kann man natürlich wiederum"
      l "eine neue Transition nach dem gleichen Schema ableiten."

      
chainedTransitionsIV :: SlideF String
chainedTransitionsIV = do
  h "Übung zu Transitionen" lightblue $ do
    p $ do
      l "Sie haben jetzt die Möglichkeit, diese"
      a "html/myD3Solutions/example2/index_v3.html" "Aufgabe"
      l "zu Transitionen zu bearbeiten."
    p $ do
      l "Mehr über Transitionen finden Sie hier:"
    pline $ a "https://github.com/mbostock/d3/wiki/Transitions" "Transitions"
    pline $ a "https://bost.ocks.org/mike/transition/" "Working with Transitions"

excercises :: SlideF String
excercises = do
  h "Übung: Farbgradienten" blueviolet $ do
    p $ l "Verschönern Sie Ihre Balken mit Farbgradienten:"
    p $ a "https://developer.mozilla.org/en-US/docs/Web/SVG/Element/linearGradient" "Lineare Gradienten"
    p $ a "https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Gradients" "Gradienten"
    p $ do
      l "Sie haben zwei Möglichkeiten:"
      l "Entweder orientiert sich der Gradient an der Höhe des Balkens"
      l "oder an der Höhe des Charts."
      l "Implementieren Sie beides."
    hint $ do
      l "Wenn sich der Gradient an der Höhe des Charts orientieren soll"
      l "hilft es, wenn Sie" >> em "gradientUnits"
      l "auf" >> em "userSpaceOnUse" >> l "setzen."

pathElement :: SlideF String
pathElement = do
  h "Das path-Element" maroon $ do
    p $ do
      l "Um beliebige Pfade zu zeichnen, eignet sich das"
      tag "path" >> l "-Element."
    p $ do
      l "Das wichtigste Attribute ist" >> attr "d" >> l "."
    p $ do
      l "Eine grundlegende Einführung in das" >> tag "path"
      l "-Element finden Sie"
      a "https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths" "hier."
    p $ do
      l "Den Wert des" >> attr "d" >> l "-Attributes händisch zu"
      l "bestimmen, ist beschwerlich."

lineFunction :: SlideF String
lineFunction = do
  h "Die Linienfunktion" maroon $ do
    p $ do
      d3js >> l "stellt mehrere Generatoren für Formen bereit,"
      l "unter anderem für Linien."
    pcode JavaScript $ do
      c "var line = d3.svg.line();"
      c "    .x(function(d, i) { return xscale(d.x); })"
      c "    .y(function(d, i) { return yscale(d.y); });"
    p $ do
      l "Mit der" >> em "line()" >> l "-Funktion können"
      l "sie bequem einen Pfad zeichnen."
    pcode JavaScript $ do
      c "var path = svg.append('path');"
      c "path.attr('d', line(dataArray));"
    p $ do
      l "Ändern sich die daten, setzen Sie" >> attr "d" >> l "neu."

clipPaths :: SlideF String
clipPaths = do
  h "Das clipPath-Element" darkred $ do
    p $ do
      l "Ragt ein Pfad oder eine andere Form"
      l "über die vorgesehene Fläche hinaus, kann es"
      l "nützlich sein, diese zu beschneiden."
    pcode HTML $ do
      c "<svg>"
      c "  <defs>"
      c "    <clipPath id='myCP'>"
      c "      <rect width='940' height='220'></rect>"
      c "    </clipPath>"
      c "  </defs>"
      c "  ..."
      c "  <path clip-path='url(#myCP)' d='...'></path>"
      c "</svg>"
    p $ do
      l "Der Pfad ist nur innerhalb des Rechtecks sichtbar."

timeScale :: SlideF String
timeScale = do
  h "Zeitskalen" seagreen $ do
    p $ do
      l "Wie auch lineare Skalen besitzen Zeitskalen die"
      l "Methoden" >> em "range()" >> l "und" >> em "domain()."
    pcode JavaScript $ do
      c "var start = new Date('2010-01');"
      c "var end = new Date('2011-01');"
      c "var xscale = d3.time.scale()"
      c "    .range([0, width-margin])"
      c "    .domain([start, end]);"
    p $ do
      l "Wer speziellere Wünsche an die Formattierung"
      l "der Labels hat, wird"
      a "https://github.com/mbostock/d3/wiki/Time-Formatting" "hier"
      l "fündig."

    
files :: SlideF String
files = do
  h "Dateien lesen" darkorange $ do
    p $ do
      d3js >> l "stellt einige Routinen zum einlesen von Datein bereit:"
    p $ l "csv, tsv, json, html, text, xml" <| [center, monospace, darkgreen]
    p $ do
      l "Eine komplette Beschreibung des API finden Sie"
      a "https://github.com/mbostock/d3/wiki/Requests" "hier."

csvFiles :: SlideF String
csvFiles = do
  h "CSV Dateien einlesen" darkorange $ do
    p $ do
      l "Die Function" >> em "d3.csv()"
      l "liest eine" >> em "csv" >> l "-Datei ein."
    pcode JavaScript $ do
      c "function parseJSON(d) { return { ... } }"
      c "function someWork(err, arrOfJSON) { ... }"
      c "d3.csv('file.csv', someWork).row(parseJSON);"
    p $ do
      l "Die Kopfzeile der Datei wird verworfen."
      l "Die einzelnen Zeilen werden zu JSON-Objekten"
      l "mit den jeweiligen Spalten-Titeln als Properties."
      l "Die Werte der Properties sind Strings,"
      l "die noch geparst werden müssen."

crossBrowserOrigin :: SlideF String
crossBrowserOrigin = do
  h "Cross Browser Origin" darkorange $ do
    p $ do
      l "Die meisten Browser verhindern es,"
      l "Dateien aus anderen Quellen zu lesen als der, von der"
      l "von der das Skript stammt."
    p $ do
      l "In Chrome können Sie die Sicherheitsvorkehrungen abschalten:"
    p $ (do
      l "$ chrome"
      l "--allow-file-access-from-files"
      linebreak
      l "--disable-web-security") <| [monospace, red]
    p $ do
      l "Firefox ist nicht ganz so streng wie Chrome."

yahooAPI :: SlideF String
yahooAPI = do
  h "Übung: CSV einlesen" darkorange $ do
    p $ do
      l "Vom" >> l "Yahoo! Finance API" <| [darkgreen, fsitalic]
      l "können Sie historische Aktienkurse herunterladen,"
      l "z.B. von Google oder IBM:"
    p $ do
      a "http://ichart.yahoo.com/table.csv?s=GOOG"
        "http://ichart.yahoo.com/table.csv?s=GOOG"
    p $ do
      l "Speichern Sie die Datei entweder auf Festplatte oder"
      l "lesen Sie sie direkt vom Server (mit Chrome)."
    hint $ do
      l "Das API erlaubt nur eine begrenzte Anzahl von"
      l "Zugriffen pro Tag pro IP-Adresse."
    p $ do
      l "Interessant sind die Spalten"
      em "Date" >> l "und" >> em "Close."
      l "Bearbeiten Sie folgende"
      a "html/myD3Solutions/stocks/index.html" "Aufgabe."

colorScales :: SlideF String
colorScales = do
  h "Farbskalen" blue $ do
    p $ do
      l "Für Farbskalen eignen sich sowohl ordinale als auch lineare Skalen."
    pcode JavaScript $ do
      c "var ord = d3.scale.ordinal()"
      c "    .domain([1, 2, 3])"
      c "    .range(['red', 'yellow', 'green']);"
    pcode JavaScript $ do
      c "var lin = d3.scale.linear()"
      c "    .domain([1, 2, 3])"
      c "    .range(['red', 'yellow', 'green']);"      
    question $ l "Was ist der Unterschied?"
    p $ do
      l "Bearbeiten Sie diese"
      a "html/myD3Solutions/colors/index.html" "Aufgabe."

viewbox :: SlideF String
viewbox = do
  h "Ausdehnung und Skalierung" green $ do
    p $ do
      l "Will man eine skalierend Graphik,"
      l "sollte man das" >> attr "viewbox" >> l "-Attibut"
      l "setzen. Auch kann man" >> attr "aspectRatio"
      l "setzen, um das Verhältnis zwischen Höhe und Breite"
      l "besser zu kontrollieren."
    p $ do
      l "Der" >> l "Internet Explorer" <| [darkgreen]
      l "kann die Ausdehnung von SVG-Graphiken"
      l "in Tabellen und Table-Layouts nicht"
      l "richtig berechnen."
      l "Hier hilft es vorher, ein" >> tag "canvas"
      l "-Element einzufügen, bei dem man ebenfalls"
      l "die gewünschte Größe setzt."
      attr "width" >> l "und" >> attr "height"
      l "des SVG-Tags setzt man dann auf" >> em "'100%'." <| [monospace]
    p $ do
      l "Es gibt einen CSS-Style"
      l "der verhindert, dass Linien in die Breite skalieren."

webpackSlide :: SlideF String
webpackSlide = do
  h "Webpack" green $ do
    p $ do
      webpack >> l "ist eine elegante Möglichkeit, JavaScript-Applikationen,"
      l "die aus mehreren Datein bestehen, zu bündeln."
    p $ do
      l "Traditionelles JavaScript kennt keine Module."
      l "Für" >> webpack >> l "gibt es einen" >> em "Loader (es2015),"
      l "der es erlaubt" >> l "EcmaScript 6" <| [darkgreen, fsitalic]
      l "zu JavaScript zu kompilieren."
      l "Damit können Sie das neue eingeführte Modul-System verwenden."
    p $ do
      webpack >> l "erlaubt es auch, CSS-Stylesheets"
      l "in Form von" >> tag "style" >> l "-Tags zu inlinen."
      l "Das ist besonders interessant, wenn man Graphik"
      l "serverseitig erzeugen will (z.B. mit" >> jsdom >> l "),"
      l "um sie dann mit" >> em "librsvg" <| [darkgreen]
      l "o.ä. zu PDF oder PNG zu transformieren."

      
{-

* viewbox-Attribut
* canvas in IE


* Referenzieren von urls im chart überall aus DOM möglich


<image x="20" y="10" width="320" height="240" xlink:href="raupen.jpg"/>
* webpack/uglify
* jsdom/serverside/nodejs
* es2015 import/export
* selectionen in variablen speichern statt nochmal select aufrufen
* Pfad-Element Punkte pushen

* Javascript

-}


theEnd :: SlideF String
theEnd = do
  h "Vielen Dank" red $ do
    pimage "html/d3_logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    para "Still not the End!" <| [center, fwbold, pt24]
    
