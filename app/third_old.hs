{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-} -- needed for ToMarkup String
{-# LANGUAGE FlexibleInstances    #-}

--module Main1 where
import Prelude hiding (head, id, div)
import GHC.Exts  (IsString (..))

data Attribute = Attribute String String
  deriving (Show)

data HtmlM a =
  Html (HtmlM a)
  | Head (HtmlM a)
  | Link a
  | Title String a
  | Body (HtmlM a)
  | Div (HtmlM a)
  | P (HtmlM a)
  | UL (HtmlM a)
  | LI (HtmlM a)
  | HRef String a
  | Table (HtmlM a)
  | Td (HtmlM a)
  | Tr (HtmlM a)
  | Th (HtmlM a)
  | forall b. Append (HtmlM b) (HtmlM a) -- Note the new value `b`
--  | Parent (HtmlM a) (HtmlM a)
--  | Leaf (HtmlM a)
  | AddAttribute Attribute (HtmlM a)
  | Content String a
  | Empty a

instance (a ~ ()) => IsString (HtmlM a) where
  fromString x = Content (fromString x) mempty

htmlValue :: HtmlM a -> a
htmlValue (Html x) =  htmlValue x
htmlValue (Head x) =  htmlValue x
htmlValue (Link x) = x
htmlValue (HRef _ x) = x
htmlValue (Title _ x) = x
htmlValue (Body x) = htmlValue x
htmlValue (Div x) = htmlValue x
htmlValue (P x) = htmlValue x
htmlValue (UL x) = htmlValue x
htmlValue (LI x) = htmlValue x
htmlValue (Table x) = htmlValue x
htmlValue (Td x) = htmlValue x
htmlValue (Tr x) = htmlValue x
htmlValue (Th x) = htmlValue x
htmlValue (Append _ x) = htmlValue x
htmlValue (AddAttribute _ x) = htmlValue x
htmlValue (Content _ x) = x
htmlValue (Empty x) = x

showValue :: (Show a) => HtmlM a -> String
showValue (Html x) =  "(Html " ++ showValue x ++ ")"
showValue (Head x) =  "(Head " ++ showValue x ++ ")"
showValue (Link x) = "(Link " ++ show x ++ ")"
showValue (HRef _ x) = "(HRef " ++ show x ++ ")"
showValue (Title t x) = "(Title " ++ t ++ show x ++ ")"
showValue (Body x) = showValue x
showValue (Div x) = showValue x
showValue (P x) = showValue x
showValue (UL x) = showValue x
showValue (LI x) = showValue x
showValue (Table x) = "(Table " ++ show (showValue x) ++ ")"
showValue (Td x) = "(Td " ++ (showValue x) ++ ")"
showValue (Tr x) = "(Tr " ++ (showValue x) ++ ")"
showValue (Th x) = "(Th " ++ (showValue x) ++ ")"
showValue (Append x y) = "(Append " ++ "_  " ++ (showValue y) ++ ")"
showValue (AddAttribute attr x) = "(AddAttribute " ++ "(Attribute " ++ (show attr) ++ ")" ++ (showValue x) ++ ")"
showValue (Content _ x) = "(Content " ++ show x ++ ")"
showValue (Empty x) = "(Empty " ++ show x ++ ")"

type Html = HtmlM ()

-- Need to update type of Append with existential
instance Functor HtmlM where
  fmap f x = Append x (Empty (f (htmlValue x)))

-- Why isn't this just an `(Empty (htmlValue x (htmlValue y)))`
-- The `homomorphism` law is not followed here.
instance Applicative HtmlM where
  pure x = Empty x

  -- Why are we not discarding the first value?
  (*>) = Append
  (<*>) x y = --Empty (htmlValue x (htmlValue y))
    Append (Append x y) (Empty (htmlValue x (htmlValue y)))

instance Monad HtmlM where
  return = pure

  m >>= f = Append m (f (htmlValue m))
  (>>) = Append


-- Update to HtmlM a to accomodate change to type of Append
renderString :: String -> HtmlM a -> String
renderString attrs (Html inner) = "<html " ++ attrs ++ ">" ++ renderString "" inner ++ "</html>"
renderString attrs (Head inner) = "<head " ++ attrs ++ ">" ++ renderString "" inner ++ "</head>"
renderString attrs (Link _) = "<link "  ++ attrs ++ ">"
renderString attrs (HRef content _) = "<a  " ++ attrs ++ ">" ++ content ++ "</a>"
renderString attrs (Title content _) = "<title  " ++ attrs ++ ">" ++ content ++ "</title>"
renderString attrs (Body content) = "<body  " ++ attrs ++ ">" ++ renderString "" content ++ "</body>"
renderString attrs (Div content) = "<div  " ++ attrs ++ ">" ++ renderString "" content ++ "</div>"
renderString attrs (P content) = "<p  " ++ attrs ++ ">" ++ renderString "" content ++ "</p>"
renderString attrs (UL content) = "<ul  " ++ attrs ++ ">" ++ renderString "" content ++ "</ul>"
renderString attrs (LI content) = "<li  " ++ attrs ++ ">" ++ renderString "" content ++ "</li>"
renderString attrs (Table content) = "<table  " ++ attrs ++ ">" ++ (renderString "" content) ++ "</table>"
renderString attrs (Tr content) = "<tr  " ++ attrs ++ ">" ++ renderString "" content ++ "</tr>"
renderString attrs (Th content) = "<th  " ++ attrs ++ ">" ++ renderString "" content ++ "</th>"
renderString attrs (Td content) = "<td  " ++ attrs ++ ">" ++ renderString "" content ++ "</td>"
renderString attrs (Append html1 html2) =
  (renderString attrs html1) ++ (renderString attrs html2)
renderString attrs (AddAttribute (Attribute k v) html) = flip renderString html $
  k ++ "\"" ++ v ++ "\""
renderString attrs (Content content _) = content
renderString attrs (Empty _) = ""

href value = Attribute "href" value
type_ value = Attribute "type" value

rel value = Attribute "rel=" value

id value = Attribute "id=" value

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable Html where
  (!) html attr = AddAttribute attr html

instance Attributable (HtmlM () -> HtmlM ()) where
  f ! attr = (\f1 -> (AddAttribute attr (f f1)))

head :: Html -> Html
head = Head

html :: Html -> Html
html = Html

title :: String -> Html
title  s = Title s ()

link :: Html
link = Link ()

body :: Html -> Html
body = Body

div :: Html -> Html
div = Div

p :: Html -> Html
p = P

ul :: Html -> Html
ul = UL

string :: String -> Html
string a = Content a ()

li :: Html -> Html
li = LI


class ToMarkup a where
    toMarkup :: a -> Html

instance ToMarkup String where
    toMarkup = string
    -- Implement escaping
    --preEscapedToMarkup = preEscapedString

page1 :: Html
page1 = html
  $ do
    head $ do
        title "Introduction page."
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        div ! id "header" $ "Syntax"
        p "This is an example of BlazeMarkup syntax."
        ul $ mapM_ (li . toMarkup . show) [1, 2, 3]

myHtml :: Html
myHtml = Html $ do
  Head $ do
    Link () ! rel "style.css" ! type_ "text/html"
    HRef "myLink" () ! href "http://read-you-a-blaze/"

main :: IO ()
main = putStrLn "Test"
-- main = do
--   putStrLn $ renderString "" $ Link ! rel "style.css" ! type_ "text/html"
--   putStrLn $ renderString "" $ Head (Append (Append (Link ! rel "style.css" ! type_ "text/html") (HRef "LinkName" ! href "http://blaze-sample.com")) (Content "content"))
