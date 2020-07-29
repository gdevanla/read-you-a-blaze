{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude  hiding ( head, id, div)
--import Text.Blaze.Html4.Strict hiding ( map )
--import Text.Blaze.Html4.Strict.Attributes  hiding ( title )

--import Text.Blaze.Renderer.Utf8  ( renderMarkup )
--import Text.Blaze.Renderer.Utf8  ( renderMarkup )

import Lib

data Attribute = Attribute String String deriving Show -- Name and Value

data Link = LinkElement String String String Link
          | Add Attribute Link
          | Empty
          deriving (Show)

href value = Attribute "href" value
type_ value = Attribute "type" value

rel value = Attribute "rel" value

(!) :: Link -> Attribute -> Link
(!) link attr = Add attr link

linkElement = LinkElement "<a" "/a>" "link to css" Empty

-- renderString :: Link -> String -> String
-- renderString link s = go
--   where
--      go :: Link -> String -> String
--      go (Add (Attribute String String) link) = flip go h $
--      go (LinkElement tag open close) = let
--        link = open ++ (go ) ++ close
--        in


-- page1 :: Markup
-- page1 = html $ do
--         head $ do
--                 title "Introduction page."
--                 link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
--         body $ do
--                 div ! id "header" $ "Syntax"
--                 p "This is an example of BlazeMarkup syntax."
--                 ul $ mapM_ (li . toMarkup . show) [1, 2, 3]

main :: IO ()
main = putStrLn "Hello"
  -- do
  -- let x = renderMarkup page1
  -- --let link' = link ! (rel "stylesheet") ! (type_ "test/css") ! (href "screen.css")
  -- putStrLn $ show x
