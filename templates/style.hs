{-# LANGUAGE OverloadedStrings #-}

import Clay

main :: IO ()
main = putCss myStylesheet

myStylesheet :: Css
myStylesheet = do
  body ? background white

  "name" ? do
    display inline
    margin-left (px 10)
  "diffPoints" ? do
    display inline
    margin-left (px 10)
  "player" ? display block