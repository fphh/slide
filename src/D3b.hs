

module D3b where



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
      l "Beide Varianten bewirken das selbe, nämlich dass"
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
      a "html/myD3Solutions/example2/index_v1.html" "Aufgabe"
      l "."
    hint $ do
      l "Benutzen Sie die x- und y-Skalen, um die x-Position bzw."
      l "Höhe Ihrer Rechtecke zu berechnen."
    hint $ do
      em "x.rangeBands()" >> l "liefert die Breite der Rechtecke zurück."
    p $ do
      l "Im Anschluss erweitern Sie Ihre Lösung"
      a "html/myD3Solutions/example2/index_v2.html" "folgendermaßen"
      l "."
      
{-
    p $
      a "html/myD3Solutions/example2/index_v3.html" "Aufgabe 3"
-}
