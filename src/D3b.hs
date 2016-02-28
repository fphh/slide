

module D3b where

{-

* Aufbau großer Graphiken in Ebenen (z-order)
* Farbverläufe
* ClipPaths
* Chained Transitions
* viewbox-Attribut
* canvas in IE


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
  ordinalScale
  barChartExample
  chainedTransitions
  chainedTransitionsII
  chainedTransitionsIII
  chainedTransitionsIV
  excercises
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
      c "var sel = d3.select('#app')"
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
      l "Sobald man z.B. eine Transition auf enter+update"
      l "anwendet, werden die Transitionen für enter"
      l "und update unterbrochen."

chainedTransitionsII :: SlideF String
chainedTransitionsII = do
  h "Transitionen aus dem Nichts" lightblue $ do
    p $ do
      l "Eine Transition kann unabhängig von einer Selektion"
      l "erzeugt werden. Nach einem Aufruf von" >> em "each()"
      l "kann man im Callback die Selektionen bearbeiten."
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
      l "auf der Selektion löst die Transition aus."


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

theEnd :: SlideF String
theEnd = do
  h "Vielen Dank" red $ do
    pimage "html/d3_logo.svg" <| [center]
    para "von Dr. Heinrich Hördegen" <| [center]
    pline (email "hoerdegen@funktional.info") <| [center, monospace, green]
    para "Not the End!" <| [center, fwbold, pt24]
    
