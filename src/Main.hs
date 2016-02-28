{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq
import Control.Monad.Free

import Control.Monad

import qualified Data.Colour as Colour
import Data.Colour (Colour)
import qualified Data.Colour.Names as CN
import qualified Data.Colour.SRGB as SRGB

import qualified Data.Aeson as Aeson

import qualified Data.Text as Text

import React.Flux

import Data.Monoid ((<>))

import qualified Slide as Slide
-- import SlideShow (slideShow)

import D3b (slideShow)


foreign import javascript safe "highlightCode()" highlightCode :: IO ()
foreign import javascript safe "$r = getLastKey()" getLastKey :: IO Int

data View = SlideView | PrintView | TableOfContentView | LinkView
  deriving (Generic, NFData)

data AppState a = AppState {
  pageNumber :: a,
  slides :: [Slide.SlideF String],
  numOfSlides :: Int,
  sView :: View }

instance Functor AppState where
  fmap f (AppState x tlk len v) = AppState (f x) tlk len v

data Action =
  JumpToPage Int
  | HighlightCode
  | KeyPressed
  | SetView View
  deriving (Generic, NFData)

instance StoreData (AppState Int) where
    type StoreAction (AppState Int) = Action
    
    transform (JumpToPage x) appState = return $
      case x of
       n | 0 <= n && n < nos -> fmap (const n) appState
       otherwise             -> appState
      where nos = numOfSlides appState
      
    transform HighlightCode appState = do
      highlightCode
      return appState

    transform KeyPressed appState = do
      lk <- getLastKey
      case lk of
       37 -> transform (JumpToPage (pageNumber appState - 1)) appState
       39 -> transform (JumpToPage (pageNumber appState + 1)) appState
       _ -> return appState

    transform (SetView v) appState = return $ appState { sView = v }
       
store :: ReactStore (AppState Int)
store = mkStore $ AppState 0 slides (length slides) SlideView
  where slides = Slide.toSlides slideShow

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a, SomeStoreAction store HighlightCode]

app :: ReactView ()
app = defineControllerView "app" store $ \state () -> do
  case sView state of
   SlideView -> slideView state
   PrintView -> printView state
   TableOfContentView -> tocView state
   LinkView -> linkView state


backToSlideShow :: ReactElementM ViewEventHandler ()
backToSlideShow =
  div_
  [ "className" $= "back-to-slides"
  , onClick $ \_ _ -> dispatch (SetView SlideView)]
  (elemText "Back to Slides")
  
linkView :: AppState Int -> ReactElementM ViewEventHandler ()
linkView (AppState _ slides _ sv) = do
  let links = concat $ concatMap extractLinks slides
      f :: (String, String) -> ReactElementM ViewEventHandler ()
      f (url, _) = div_ [] $ do
        a_ ["href" $= Text.pack url, "target" $= "_blank" ] (elemText url)
  backToSlideShow      
  mapM_ f links
  backToSlideShow
  

extractLinks (Free (Slide.Slide attrs hdr paras (Pure _))) =
  extractLinksPara paras
extractLinks _ = return []


extractLinksPara (Free (Slide.Paragraph (Slide.Image url) attrs ws k)) = do
  xs <- extractLinksWord ws
  ys <- extractLinksPara k
  return $ (url, "Image") : xs ++ ys
extractLinksPara (Free (Slide.Paragraph _ attrs ws k)) = do
  xs <- extractLinksWord ws
  ys <- extractLinksPara k
  return (xs ++ ys)
extractLinksPara _ = return []
  
extractLinksWord (Free (Slide.Word (Slide.Link url) attrs w k)) = do
  xs <- extractLinksWord k
  return $ (url, w):xs
extractLinksWord (Free (Slide.Word _ attrs w k)) =
  extractLinksWord k
extractLinksWord _ = return []



tocView :: AppState Int -> ReactElementM ViewEventHandler ()
tocView (AppState _ slides _ sv) = do
  let idx = [0..]
      cb page _ _ =
        [SomeStoreAction store (JumpToPage page)]
        ++ dispatch (SetView SlideView)
      f 0 _ = h3_ [] (elemText "Table of Content")
      f page s = div_ [ "className" $= "tableOfContent"] $ do
        div_ [ "className" $= "toc-page-number"] (elemText (show (page+1) ++ "."))
        toTocItem page sv s
  backToSlideShow
  zipWithM_ f idx slides
  backToSlideShow

toTocItem :: Int -> View -> Slide.SlideF String -> ReactElementM ViewEventHandler ()
toTocItem page sv (Free (Slide.Slide attrs hdr paras (Pure _))) =
  let cb page _ _ =
        [SomeStoreAction store (JumpToPage page)]
        ++ dispatch (SetView SlideView)
  in span_ [ "className" $= "toc-item"
           , onClick (cb page) 
           ] (toWord hdr)
toTocItem _ _ _ = return ()


slideView :: AppState Int -> ReactElementM ViewEventHandler ()
slideView state = do                     
  div_ [ "id" $= "slide"
       , onKeyUp $ \_ _ -> dispatch KeyPressed ] $ do
    content state
    index state
  footer state

printView :: AppState Int -> ReactElementM ViewEventHandler ()
printView (AppState _ slides _ sv) = do
  let idx = [0..]
      cb page _ _ =
        [SomeStoreAction store (JumpToPage page)]
        ++ dispatch (SetView SlideView)
      f page s = div_ [ "className" $= "print-slide" ] $ do
        div_ [ "className" $= "page-number"
             , onClick (cb page) ] (elemText ("Slide " ++ show (page+1)))
        toSlide sv s
  backToSlideShow
  zipWithM_ f idx slides
  backToSlideShow

toWord :: Slide.WordF String -> ReactElementM ViewEventHandler ()
toWord (Pure _) = return ()
toWord (Free (Slide.Word Slide.LineBreak attrs w k)) = do
  br_ []
  toWord k
toWord (Free (Slide.Word (Slide.Link url) attrs w k)) = do
  span_ [ "className" $= "word" ] $
    span_ [ "style" @= attrs ] $
      a_ ["href" $= Text.pack url, "target" $= "_blank" ] (elemText w)
  toWord k
toWord (Free (Slide.Word _ attrs w k)) = do
  span_ [ "className" $= "word"] $
    span_ ["style" @= attrs ] (elemText w)
  toWord k

toLine :: Slide.WordF String -> ReactElementM ViewEventHandler ()
toLine (Pure _) = return ()
toLine (Free (Slide.Word _ attrs w k)) = do
  span_ [ "style" @= attrs ] (elemText w)
  br_ []
  toLine k

toParagraph :: Slide.ParagraphF String -> ReactElementM ViewEventHandler ()
toParagraph (Pure _) = return ()
toParagraph (Free (Slide.Paragraph Slide.Text attrs ws k)) = do
  p_ [ "className" $= "paragraph-text", "style" @= attrs ] (toWord ws)
  toParagraph k
toParagraph (Free (Slide.Paragraph (Slide.Code language) attrs ws k)) = do
  pre_ $ code_ [ "className" $= (Text.pack $ show language)
               , "style" @= attrs ] (toLine ws)
  toParagraph k
toParagraph (Free (Slide.Paragraph (Slide.Image url) attrs ws k)) = do
  p_ [ "className" $= "paragraph-image", "style" @= attrs ] $
    img_ [ "src" $= Text.pack url ] (return ())
  toParagraph k


toSlide :: View -> Slide.SlideF String -> ReactElementM ViewEventHandler ()
toSlide sv (Free (Slide.Slide attrs hdr paras (Pure _))) = do
  let cls = case sv of
        SlideView -> "content"
        PrintView -> "content-print"
  h2_ [ "className" $= "header" ] (toWord hdr)
  div_ [ "className" $= cls, "style" @= attrs] $
    div_ ["className" $= "inner-content" ](toParagraph paras)
toSlide _ _ = return ()

content :: AppState Int -> ReactElementM ViewEventHandler ()
content (AppState x slides _ sv) = toSlide sv (slides !! x)

index :: AppState Int -> ReactElementM ViewEventHandler ()
index (AppState x ss _ _) =
  let idx = [0 .. length ss-1]
      f y =
        li_ [ "className" $= (if x==y then "selected" else "not-selected")
            , onClick $ \_ _ -> dispatch (JumpToPage y) 
            ] (elemShow (y+1))
  in div_ [ "className" $= "nav" ] $
     ul_ $ do
       mapM_ f idx
       li_ [ onClick $ \_ _ -> dispatch (JumpToPage (x-1)) ] "◄"
       li_ [ onClick $ \_ _ -> dispatch (JumpToPage (x+1)) ] "►"

footer :: AppState Int -> ReactElementM ViewEventHandler ()
footer _ = do
  div_ [ "className" $= "footer" ] $
     ul_ $ do
       li_ [onClick $ \_ _ -> dispatch (SetView PrintView)] (elemText "Print View")
       li_ [onClick $ \_ _ -> dispatch (SetView TableOfContentView)] (elemText "Table of Content")
       li_ [onClick $ \_ _ -> dispatch (SetView LinkView)] (elemText "Links")

main :: IO ()
main = do
  reactRender "app" app ()
