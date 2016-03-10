
module Style where

import Control.Monad.Free

import qualified Data.List as List


import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTy

import qualified Data.Colour as Colour
import Data.Colour (Colour)
import qualified Data.Colour.Names as CN
import qualified Data.Colour.SRGB as SRGB

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.HashMap.Strict as HM


class Style a where
  style :: (Text, Aeson.Value) -> Free a () -> Free a ()

type StyleAttrs = Aeson.Object

emptyAttrs :: StyleAttrs
emptyAttrs = HM.empty

type StyleValue = Aeson.Value

(.=) :: AesonTy.ToJSON a => String -> a -> AesonTy.Pair
a .= b = Text.pack a Aeson..= b

color :: Colour Double -> (Text, StyleValue)
color c = "color" .= SRGB.sRGB24show c

red, blue, green, white, black :: (Text, StyleValue)
white = color CN.white
red = color CN.red
yellow = color CN.yellow
azure = color CN.azure
blueviolet = color CN.blueviolet
blue = color CN.blue
deepskyblue = color CN.deepskyblue
dodgerblue = color CN.dodgerblue
darkblue = color CN.darkblue
darkred = color CN.darkred
maroon = color CN.maroon
green = color CN.green
darkgreen = color CN.darkgreen
black = color CN.black
pink = color CN.pink
lightblue = color CN.lightblue
orange = color CN.orange
darkorange = color CN.darkorange
darksalmon = color CN.darksalmon
crimson = color CN.crimson
seagreen = color CN.seagreen
magenta = color CN.magenta

bgcolor :: Colour Double -> (Text, StyleValue)
bgcolor c = "backgroundColor" .= SRGB.sRGB24show c

bgred, bgblue, bggreen, bgwhite, bgblack :: (Text, StyleValue)
bgred = bgcolor CN.red
bgblue = bgcolor CN.blue
bggreen = bgcolor CN.green
bgwhite = bgcolor CN.white
bgblack = bgcolor CN.black
bgpink = bgcolor CN.pink
bglightblue = bgcolor CN.lightblue
bglightgreen = bgcolor CN.lightgreen
bgdarksalmon = bgcolor CN.darksalmon
bgcrimson = bgcolor CN.crimson
bgseagreen = bgcolor CN.seagreen
bgdarkblue = bgcolor CN.darkblue


fontSize :: Int -> (Text, StyleValue)
fontSize fs = "fontSize" .= (show fs ++ "pt")

pt6, pt8, pt10, pt12, pt16, pt20, pt24, pt36 :: (Text, StyleValue)
pt6 = fontSize 8
pt8 = fontSize 8
pt10 = fontSize 10
pt12 = fontSize 12
pt16 = fontSize 16
pt20 = fontSize 20
pt24 = fontSize 24
pt32 = fontSize 32
pt36 = fontSize 36
pt48 = fontSize 48

data FontStyle = Normal | Italic | Oblique deriving (Show)

fontStyle :: FontStyle -> (Text, StyleValue)
fontStyle fs = "fontStyle" .= show fs

fsnormal, fsitalic, fsoblique :: (Text, StyleValue)
fsnormal = fontStyle Normal
fsitalic = fontStyle Italic
fsoblique = fontStyle Oblique

data FontFamily = Verdana | MonoSpace | Cursive | Fantasy deriving (Show)

fontFamily :: FontFamily -> (Text, StyleValue)
fontFamily ff = "fontFamily" .= show ff

verdana, monospace, cursive, fantasy :: (Text, StyleValue)
verdana = fontFamily Verdana
monospace = fontFamily MonoSpace
cursive = fontFamily Cursive
fantasy = fontFamily Fantasy

data FontWeight =
  FWNormal | FWBold | FWLighter | FWBolder |
  FW100 | FW200 | FW300 | FW400 | FW500 | FW600 | FW700 | FW800 | FW900

instance Show FontWeight where
  show fw =
    case fw of
     FWNormal -> "normal"
     FWBold -> "bold"
     FWLighter -> "lighter"
     FWBolder -> "bolder"
     FW100 -> "100"
     FW200 -> "200"
     FW300 -> "300"
     FW400 -> "400"
     FW500 -> "500"
     FW600 -> "600"
     FW700 -> "700"
     FW800 -> "800"
     FW900 -> "900"
 

fontWeight :: FontWeight -> (Text, StyleValue)
fontWeight ff = "fontWeight" .= show ff

fwbold, fwnormal, fwlighter, fwbolder :: (Text, StyleValue)
fwbold = fontWeight FWBold
fwnormal = fontWeight FWNormal
fwlighter = fontWeight FWLighter
fwbolder = fontWeight FWBolder


data TextDecoration = Underline deriving (Show)

textDecoration :: TextDecoration -> (Text, StyleValue)
textDecoration td = "textDecoration" .= show td

underline :: (Text, StyleValue)
underline = textDecoration Underline


data Align = L | C | R

instance Show Align where
  show L = "left"
  show C = "center"
  show R = "right"

align :: Align -> (Text, StyleValue)
align a = "textAlign" .= show a

left, right, center :: (Text, StyleValue)
left = align L
center = align C
right = align R

data VAlign = T | B | M

instance Show VAlign where
  show T = "top"
  show B = "bottom"
  show M = "middle"

valign :: VAlign -> (Text, StyleValue)
valign a = "verticalAlign" .= show a

top, bottom, middle :: (Text, StyleValue)
top = valign T
bottom = valign B
middle = valign M


(<|) ::
  (Style s, Foldable t) =>
  Free s () -> t (Text, StyleValue) -> Free s ()
x <| attrs = List.foldr style x attrs


radius :: Integer -> (Text, StyleValue)
radius n = "borderRadius" .= show n

paddingL, paddingR :: Integer -> (Text, StyleValue)
paddingL n = "paddingLeft" .= show n
paddingR n = "paddingRight" .= show n
paddingT n = "paddingTop" .= show n
paddingB n = "paddingBottom" .= show n


width :: Integer -> (Text, StyleValue)
width n = "width" .= show n


data Maring = Auto deriving (Show)

marginL, marginR :: (Show a) => a -> (Text, StyleValue)
marginL n = "marginLeft" .= show n
marginR n = "marginRight" .= show n
