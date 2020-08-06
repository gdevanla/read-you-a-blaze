{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances    #-}


import           Prelude                 hiding ( head
                                                , id
                                                , div
                                                )
import           GHC.Exts                       ( IsString(..) )

data Attribute = Attribute String String
  deriving (Show)

-- Note change in type from first.hs
data HtmlM a = Html (HtmlM a)
  | Head (HtmlM a)
  | Body (HtmlM a)
  | Link a
  | Title String a
  | HRef String a
  | Div (HtmlM a)
  | UL (HtmlM a)
  | LI (HtmlM a)
  | P (HtmlM a)
  | forall b. Append (HtmlM b) (HtmlM a)
  | AddAttribute Attribute (HtmlM a)
  | Content String a
  | Empty a

type Html = HtmlM ()
type Markup = HtmlM ()

instance (a ~ ()) => IsString (HtmlM a) where
        fromString x = Content (fromString x) mempty


-- Walk through the Html data structure to return the embedded value. Used in Functor, Applicative and Monad instances
htmlValue :: HtmlM a -> a
htmlValue (Html x          ) = htmlValue x
htmlValue (Head x          ) = htmlValue x
htmlValue (Link x          ) = x
htmlValue (HRef _ x) = x
htmlValue (Div x) = htmlValue x
htmlValue (LI x) = htmlValue x
htmlValue (UL x) = htmlValue x
htmlValue (P x) = htmlValue x
htmlValue (Append       _ x) = htmlValue x
htmlValue (AddAttribute _ x) = htmlValue x
htmlValue (Content      _ x) = x
htmlValue (Empty x         ) = x

--- Same as the version in first.hs
renderMarkup :: String -> HtmlM a -> String
renderMarkup attrs (Html inner) =
        "<html" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</html>"
renderMarkup attrs (Head inner) =
        "<head" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</head>"
renderMarkup attrs (Body inner) =
        "<body" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</body>"
renderMarkup attrs (P inner) =
        "<p" ++ attrs ++ ">" ++ renderMarkup "" inner ++ "</p>"
renderMarkup attrs (Link _) = "<link" ++ attrs ++ ">"
renderMarkup attrs (Title s _ ) = "<title" ++ attrs ++ ">" ++ s ++ "</title>"
renderMarkup attrs (HRef content _) = "<a" ++ attrs ++ ">" ++ content ++ "</a>"
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
renderMarkup attrs (Content content _) = content
renderMarkup attrs (Empty _) = ""

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
toMarkup s = Content s ()

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


-- HTML helpers
html :: Html -> Html
html = Html

head :: Html -> Html
head = Head

link :: Html
link = Link ()

title :: String -> Html
title s = Title s ()

div :: Html -> Html
div = Div

p :: Html -> Html
p = P

ul :: Html -> Html
ul = UL

li :: Html -> Html
li = LI

body :: Html -> Html
body = Body


-- We need to support the (!) operator as (!) and another version of it as `mult`'
--(!) :: Html -> Attribute -> Html
--(!) html attr = AddAttribute attr html

mult :: (Html -> Html) -> Attribute -> Html -> Html
mult html1 attr = \html2 -> AddAttribute attr (html1 html2)

foldHtml :: [Html] -> Html
foldHtml (x:xs) = Append x (foldHtml xs)
foldHtml [] = Empty ()

-- prior to adding attributable interface
-- page1 :: Html
-- page1 = html $ do
--         head $ do
--                 link ! rel "style.css" ! type_ "text/html"
--                 title "Introduction page."
--         body $ do
--                 mult Div (id "header") $ "Syntax"
--                 p $ "This is an example of Blazemarkup syntax."
--                 ul $ mapM_ (li . toMarkup . show) [1, 2, 3]

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable (Html) where
  (!) html attr = AddAttribute attr html

instance Attributable (Html -> Html ) where
  f ! attr = \f1 -> AddAttribute attr $ f f1

page1 :: Html
page1 = html $ do
        head $ do
                link ! rel "style.css" ! type_ "text/html"
                title "Introduction page."
        body $ do
                Div ! (id "header") $ "Syntax"
                p $ "This is an example of Blazemarkup syntax."
                ul $ mapM_ (li . toMarkup . show) [1, 2, 3]


main :: IO ()
main = do
  putStrLn $ renderMarkup "" page1
