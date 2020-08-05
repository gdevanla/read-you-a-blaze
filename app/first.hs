{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import           Prelude                 hiding ( head
                                                , id
                                                , div
                                                )

data Attribute = Attribute String String
  deriving (Show)

data Html = HTML Html
  | Head Html
  | Body Html
  | Link
  | Title String
  | HRef String
  | Div Html
  | UL Html
  | LI Html
  | P Html
  | Append Html Html
  | AddAttribute Attribute Html
  | Content String
  | Empty
  deriving (Show)


renderMarkup :: String -> Html -> String
renderMarkup attrs (HTML inner) =
        "<html" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</html>"
renderMarkup attrs (Head inner) =
        "<head" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</head>"
renderMarkup attrs (Body inner) =
        "<body" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</body>"
renderMarkup attrs (P inner) =
        "<p" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</p>"
renderMarkup attrs Link = "<link" ++ attrs ++ ">"
renderMarkup attrs (Title s) = "<title" ++ attrs ++ ">" ++ s ++ "</title>"
renderMarkup attrs (HRef content) = "<a" ++ attrs ++ ">" ++ content ++ "</a>"
renderMarkup attrs (Div content) =
        "<div" ++ attrs ++ ">" ++ (renderMarkup "" content) ++ "</div>"
renderMarkup attrs (UL content) =
        "<ul" ++ attrs ++ ">" ++ (renderMarkup "" content) ++ "</ul>"
renderMarkup attrs (LI content) =
        "<li" ++ attrs ++ ">" ++ (renderMarkup "" content) ++ "</li>"
renderMarkup attrs (Append html1 html2) =
  (renderMarkup attrs html1) ++ (renderMarkup attrs html2)
renderMarkup attrs (AddAttribute (Attribute k v) html) = flip renderMarkup html $
  " " ++ k ++ "\"" ++ v ++ "\""
renderMarkup attrs (Content content) = content
renderMarkup attrs Empty = ""

href :: String -> Attribute
href value = Attribute "href" value

type_ :: String -> Attribute
type_ value = Attribute "type" value

-- -- Attribute related functions
id :: String -> Attribute
id value = Attribute "id" value

rel :: String -> Attribute
rel value = Attribute "rel=" value

toMarkup :: String -> Html
toMarkup s = Content s


-- HTML helpders
-- link :: Html
-- link = Link

-- title :: String -> Html
-- title s = Title s

-- head :: Html -> Html
-- head = HTML

-- div :: Html -> Html
-- div = Div

-- p :: Html -> Html
-- p = P

-- ul :: Html -> Html
-- ul = UL

-- li :: Html -> Html
-- li = LI

-- body = Html -> Html
-- body = Body

-- html = Html -> Html
-- html = HTML

-- We need to support the (!) operator as (!) and another version of it as `mult`'
(!) :: Html -> Attribute -> Html
(!) html attr = AddAttribute attr html

mult :: (Html -> Html) -> Attribute -> Html -> Html
mult html1 attr = \html2 -> AddAttribute attr (html1 html2)

foldHtml :: [Html] -> Html
foldHtml (x:xs) = Append x (foldHtml xs)
foldHtml [] = Empty


page1 :: Html
page1 = let
  link' = Link ! rel "style.css" ! type_ "text/html"
  title' = Title "Introduction page."
  head' = Head (Append link' title')
  div' = mult Div (id "header") $ (Content "Syntax")
  p' = P $ Content "This is an example of Blazemarkup syntax."
  ul' = UL $ foldHtml $ (map (LI . toMarkup . show) [1, 2, 3])
  body' = Body (Append (Append div' p') ul')
  html' = HTML $ Append head' body'
  in
    html'

main :: IO ()
main = do
  putStrLn $ renderMarkup "" page1

  -- putStrLn $ renderMarkup "" $ Head
  --   (Append
  --     (Append (Link ! rel "style.css" ! type_ "text/html") (HRef "LinkName" ! href "http://blaze-sample.com")) (Content "content"))
