{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

--module Main1 where

data Attribute = Attribute String String
  deriving (Show)

data HtmlM a =
  Html (HtmlM a)
  | Head (HtmlM a)
  | Link a
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


htmlValue :: HtmlM a -> a
htmlValue (Html x) =  htmlValue x
htmlValue (Head x) =  htmlValue x
htmlValue (Link x) = x
htmlValue (HRef _ x) = x
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
showValue (HRef _ x) = "HRef " ++ show x ++ ")"
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

(!) :: Html -> Attribute -> Html
(!) html attr = AddAttribute attr html

--linkElement = Link (Empty ())

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
