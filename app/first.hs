{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--module Main1 where
import Prelude hiding ((!!))

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
  -- | Table Html
  -- | Td Html
  -- | Tr Html
  -- | Th Html
  | Append Html Html
--  | Parent Html Html
--  | Leaf Html
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

-- renderMarkup attrs (Table content) = "<table  " ++ attrs ++ ">" ++ (renderMarkup "" content) ++ "</table>"
-- renderMarkup attrs (Tr content) = "<tr  " ++ attrs ++ ">" ++ renderMarkup "" content ++ "</tr>"
-- renderMarkup attrs (Th content) = "<th  " ++ attrs ++ ">" ++ renderMarkup "" content ++ "</th>"
-- renderMarkup attrs (Td content) = "<td  " ++ attrs ++ ">" ++ renderMarkup "" content ++ "</td>"

renderMarkup attrs (Append html1 html2) =
  (renderMarkup attrs html1) ++ (renderMarkup attrs html2)
renderMarkup attrs (AddAttribute (Attribute k v) html) = flip renderMarkup html $
  " " ++ k ++ "\"" ++ v ++ "\""
renderMarkup attrs (Content content) = content
renderMarkup attrs Empty = ""

href value = Attribute "href" value
type_ value = Attribute "type" value
id_ value = Attribute "id" value

toMarkup :: String -> Html
toMarkup s = Content s

rel value = Attribute "rel=" value

(!) :: Html -> Attribute -> Html
(!) html attr = AddAttribute attr html

mult :: (Html -> Html) -> Attribute -> Html -> Html
mult html1 attr = \html2 -> AddAttribute attr (html1 html2)

linkElement = Link

foldHtml :: [Html] -> Html
foldHtml (x:xs) = Append x (foldHtml xs)
foldHtml [] = Empty


main :: IO ()
main = do
  let link' = Link ! rel "style.css" ! type_ "text/html"
  let title' = Title "Introduction page."
  let head' = Head (Append link' title')
  let div' = mult Div (id_ "header") $ (Content "Syntax")
  let p' = P $ Content "This is an example of Blazemarkup syntax."
  let ul' = UL $ foldHtml $ (map (LI . toMarkup . show) [1, 2, 3])
  let body' = Body (Append (Append div' p') ul')
  let html = HTML $ Append head' body'
  putStrLn $ renderMarkup "" html

  -- putStrLn $ renderMarkup "" $ Head
  --   (Append
  --     (Append (Link ! rel "style.css" ! type_ "text/html") (HRef "LinkName" ! href "http://blaze-sample.com")) (Content "content"))
