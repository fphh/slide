{-# LANGUAGE DeriveFunctor #-}

module Slide where

import Control.Monad.Free

import qualified Data.List as List

import Prelude hiding (Word)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTy

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.HashMap.Strict as HM

import Style

type Url = String

data WordType =
  SomeWord
  | Link Url
  | LineBreak
  deriving (Show)

data Word a k = Word {
  type_ :: WordType,
  style_ :: StyleAttrs,
  word_ :: a,
  cont_ :: k } deriving (Functor, Show)

type WordF a = Free (Word a) ()

instance Style (Word a) where
  style (k, v) (Free (Word wt s w c)) = Free (Word wt newSty w c')
    where newSty =
            case HM.lookup k s of
             Nothing -> HM.insert k v s
             _ -> s
          c' = style (k, v) c
  style _ x = x

wordT :: WordType -> a -> Free (Word a) ()
wordT wt str = Free (Word wt emptyAttrs str (Pure ()))

word :: a -> Free (Word a) ()
word = wordT SomeWord

line :: String -> Free (Word String) ()
line = mapM_ (wordT SomeWord) . words

code :: a -> Free (Word a) ()
code = wordT SomeWord

verbatim :: a -> Free (Word a) ()
verbatim = wordT SomeWord

linebreak :: Free (Word String) ()
linebreak = wordT LineBreak ""

link :: Url -> a -> Free (Word a) ()
link url = wordT (Link url)

email :: String -> Free (Word String) ()
email url = link ("mailto:" ++ url) url

data Language = HTML | JavaScript | Haskell | CLang | CSS | Directory

instance Show Language where
  show HTML = "html"
  show JavaScript = "javascript"
  show Haskell = "haskell"
  show CLang = "cpp"
  show CSS = "css"
  show _ = "none"

data ParagraphType =
  Text
  | Code Language
  | Image Url
  deriving (Show)

data Paragraph a k = Paragraph {
  ptype_ :: ParagraphType,
  pstyle_ :: StyleAttrs,
  pword_ :: Free (Word a) (),
  pcont_ :: k } deriving (Show, Functor)

instance Style (Paragraph a) where
  style (k, v) (Free (Paragraph t s w c)) = Free (Paragraph t newSty w c')
    where newSty =
            case HM.lookup k s of
             Nothing -> HM.insert k v s
             _ -> s
          c' = style (k, v) c
  style _ x = x

type ParagraphF a = Free (Paragraph a) ()

pline :: Free (Word a) () -> Free (Paragraph a) ()
pline w = Free (Paragraph Text emptyAttrs w (Pure ()))

pcode :: Language -> Free (Word a) () -> Free (Paragraph a) ()
pcode l w = Free (Paragraph (Code l) emptyAttrs w (Pure ()))

pimage :: Url -> Free (Paragraph a) ()
pimage url = Free (Paragraph (Image url) emptyAttrs (Pure ()) (Pure ()))


data Slide a k = Slide {
  sstyle_ :: StyleAttrs,
  sheader_ :: Free (Word a) (),
  sparagraph_ :: Free (Paragraph a) (),
  scont_ :: k } deriving (Show, Functor)

type SlideF a = Free (Slide a) ()

instance Style (Slide a) where
  style (k, v) (Free w) = Free (w { sstyle_ = HM.insert k v (sstyle_ w) })
  style _ x = x

slide :: Free (Word a) () -> Free (Paragraph a) () -> Free (Slide a) ()
slide h p = Free (Slide emptyAttrs h p (Pure ()))

numberOfSlides :: SlideF a -> Int
numberOfSlides (Pure _) = 0
numberOfSlides (Free (Slide _ _ _ k)) = 1 + numberOfSlides k

toSlides :: SlideF a -> [SlideF a]
toSlides (Pure _) = []
toSlides (Free (Slide attrs h w k)) =
  Free (Slide attrs h w (Pure ())) : toSlides k
