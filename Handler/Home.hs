{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    let optional    = $(widgetFile "diff")
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    let optional    = $(widgetFile "diff")
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getGamedayR :: Int -> Handler RepHtml
getGamedayR gameNo = do
  let gameday = gameNo
      file 25 = $(widgetFile "gameday25")
      file 24 = $(widgetFile "gameday25")
      file 23 = $(widgetFile "gameday25")
      file 22 = $(widgetFile "gameday25")
      optional = file gameNo
  defaultLayout $ do
     setTitle "Punktestand des Spieltages"
     $(widgetFile "gameday")
