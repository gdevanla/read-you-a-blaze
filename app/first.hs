{-# LANGUAGE OverloadedStrings #-}

--module Main1 where

data Attribute = Attribute String String
  deriving (Show)

data Html = Head Html
  | Link
  | HRef String
  | Table Html
  | Td Html
  | Tr Html
  | Th Html
  | Append Html Html
--  | Parent Html Html
--  | Leaf Html
  | AddAttribute Attribute Html
  | Content String
  | Empty
  deriving (Show)


renderString :: String -> Html -> String
renderString attrs (Head inner) = "<head " ++ attrs ++ ">" ++ renderString "" inner ++ "</head>"
renderString attrs Link = "<link "  ++ attrs ++ ">"
renderString attrs (HRef content) = "<a  " ++ attrs ++ ">" ++ content ++ "</a>"
renderString attrs (Table content) = "<table  " ++ attrs ++ ">" ++ (renderString "" content) ++ "</table>"
renderString attrs (Tr content) = "<tr  " ++ attrs ++ ">" ++ renderString "" content ++ "</tr>"
renderString attrs (Th content) = "<th  " ++ attrs ++ ">" ++ renderString "" content ++ "</th>"
renderString attrs (Td content) = "<td  " ++ attrs ++ ">" ++ renderString "" content ++ "</td>"
renderString attrs (Append html1 html2) =
  (renderString attrs html1) ++ (renderString attrs html2)
renderString attrs (AddAttribute (Attribute k v) html) = flip renderString html $
  k ++ "\"" ++ v ++ "\""
renderString attrs (Content content) = content
renderString attrs Empty = ""

href value = Attribute "href" value
type_ value = Attribute "type" value

rel value = Attribute "rel=" value

(!) :: Html -> Attribute -> Html
(!) html attr = AddAttribute attr html

linkElement = Link

main :: IO ()
main = do
  putStrLn $ renderString "" $ Link ! rel "style.css" ! type_ "text/html"
  putStrLn $ renderString "" $ Head (Append (Append (Link ! rel "style.css" ! type_ "text/html") (HRef "LinkName" ! href "http://blaze-sample.com")) (Content "content"))
