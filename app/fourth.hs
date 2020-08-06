{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-} -- needed for ToMarkup String
{-# LANGUAGE FlexibleInstances    #-}

--module Main1 where
import Prelude hiding (head, id, div)
import GHC.Exts  (IsString (..))

data Attribute = Attribute String String
  deriving (Show)

type Tag = String
type Open = String
type Close = String
type Key = String
type Value = String

data MarkupM a
  = Parent Tag Open Close (MarkupM a)
  | Leaf Tag Open Close a
  | Content String a
  | forall b. Append (MarkupM b) (MarkupM a)
  | AddAttribute Attribute (MarkupM a)
  | Empty a


instance (a ~ ()) => IsString (MarkupM a) where
  fromString x = Content (fromString x) mempty

markupValue :: MarkupM a -> a
markupValue (Parent _ _ _ x) = markupValue x
markupValue (Leaf _ _ _ x) = x
markupValue (Content _ x) = x
markupValue (Append _ m1) = markupValue m1
markupValue (AddAttribute _  m1) = markupValue m1
markupValue (Empty x) = x

type Markup = MarkupM ()

-- Need to update type of Append with existential
instance Functor MarkupM where
  fmap f x = Append x (Empty (f (markupValue x)))

-- Why isn't this just an `(Empty (htmlValue x (htmlValue y)))`
-- The `homomorphism` law is not followed here.
instance Applicative MarkupM where
  pure x = Empty x

  -- Why are we not discarding the first value?
  (*>) = Append
  (<*>) x y = --Empty (htmlValue x (htmlValue y))
    Append (Append x y) (Empty (markupValue x (markupValue y)))

instance Monad MarkupM where
  return = pure

  m >>= f = Append m (f (markupValue m))
  (>>) = Append


renderString :: String -> MarkupM a -> String
renderString attrs (Parent tag open close content) = open ++ attrs ++ ">" ++ (renderString "" content) ++ close
renderString attrs (Leaf _ begin end _) = begin ++ attrs ++ end
renderString attrs (Content x _) = x
renderString attrs (Append m1 m2) =
  renderString attrs m1 ++ renderString attrs m2
renderString attrs (AddAttribute (Attribute key value) m1) = flip renderString m1 $
                   " " ++ key ++ value ++ "\"" ++ attrs
renderString attrs (Empty x) = ""


href value = Attribute "href=\"" value
type_ value = Attribute "type=\"" value

rel value = Attribute "rel=\"" value

id value = Attribute "id=\"" value

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable Markup where
  (!) html attr = AddAttribute attr html

instance Attributable (Markup -> Markup) where
  f ! attr = \f1 -> AddAttribute attr $ f f1
  --f ! attr = AddAttribute attr $ f

type Html = MarkupM ()

html :: Html -> Html
html = Parent "html" "<html" "</head>"

head :: Html -> Html
head = Parent "head" "<head" "</head>"

title :: Html -> Html
title = Parent "title" "<title" "</title>"

link :: Html
link = Leaf "link" "<link" ">" ()

body :: Html -> Html
body = Parent "body" "<body" "</body>"

div :: Html -> Html
div = Parent "div" "<div" "</div>"

p :: Html -> Html
p = Parent "p" "<p" "</p>"

ul :: Html -> Html
ul = Parent "ul" "<ul" "</ul>"

li :: Html -> Html
li = Parent "li" "<li" "</li>"

string :: String -> Html
string a = Content a ()

class ToMarkup a where
    toMarkup :: a -> Markup

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

main :: IO ()
main = putStrLn $ renderString "" page1
