

module SlideShow where

import Style
import Slide

import D3


testLine :: WordF String
testLine =
  (line "hello" >> word "you!" <| [red] >> (link "http://www.playboy.com" " Yes, you!") <| [fantasy])

para2 :: ParagraphF String
para2 = do
  pline testLine
  pline (do
    link "https://www.bahn.de" "This item is" <| [fwbold]
    word "red" <| [red, bgblue, pt12]
    line ",isn't it?") <| [bggreen, center]
  (pline $ do
    (line "This item is " >> word "not red" <| [bgblack, green, underline]) <| [monospace]
    line "Noch eine Zeile (blue)" <| [bgblue, white]
    line "This should be red") <| [bgred]
  pline testLine <| [fsitalic, monospace, pt8]

slideShow :: SlideF String
slideShow = do
  titlePage
  directory
  introD3
  introDOM
  slide (word "Header 1" <| [red, pt36]) $ do
    para2
    pcode JavaScript $ do
      code "function f(x) {"
      code "    return x;"
      code "}"
      code "f(10)" <| [underline]
  slide  (word "Noch ein Header" <| [green, pt36]) $ do
    pline $ line "number 2 asd ads das df d dsf das ads adsfadsfas fddfa fdasdas fadsf ads asdf adsf fa asfd afdsafds asfd asfadsf"
    para2

  slide (line "Image Test") $ do
    pimage "d3_logo.svg" <| [center, middle]
    pimage "http://www.pictures-magazin.de/wp-content/uploads/2016/01/16_X-Pro2_BK_FrontLeft_56mm_1500x1060-218x135.jpg" <| [right]
  slide testLine $ do
    pcode JavaScript $ do
      code "function f(x) {"
      code "    for (var i=0; i<10; i++) {"
      code "        if (i%2 == 0) {"
      code "            console.log(0);"
      code "        } else {"
      code "            console.log('hello');" <| [bgblack, red]
      code "        }"
      code "    }"
      code ""
      code "    return" >> word "undefined;" <| [underline]
      code "}"
  slide (testLine <| [pt36, underline]) $ do
    (pcode Haskell $ do
      code "let x = 5"
      code "    g y = 2 * y"
      code "in g $ x + 7") <| [bgblack, white]
    pcode CLang $ do
      code "int f(double x)"
      code "{"
      code "    return x*2;"
      code "}"
  slide (line "html test" <| [fwbold, red]) $ do
    pcode HTML $ do
      code "<!DOCTYPE html>"
      code ""
      code "<html>"
      code "  <head></head>"
      code "  <body>"
      code "    <h1>Test</h1>"
      code "    <br/>"
      code "  </body>"
      code "</html>"
