{-# LANGUAGE FlexibleInstances #-}

module Imports.Parser.ParseComunio where

import Network.Browser
import Network.HTTP
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Data.List (delete)
import Data.String.Unicode
import Prelude
import Control.Monad (MonadPlus (..))

---------- global read/write configs ----------
writeOptions :: [SysConfig]
writeOptions = [ withIndent yes, withOutputXHTML
               , withAddDefaultDTD yes, withOutputEncoding utf8
               ]

readOptions :: [SysConfig]
readOptions =  [withParseHTML yes, withInputEncoding utf8, withWarnings no]

--------- shortcut selectors for testing ----------
readSrc :: String -> IOXmlTree
readSrc filePath =
  readDocument readOptions filePath

src :: String -> IOXmlTree
src str = readSrc (str ++ ".html")

--------------------------
---------- URLs ----------
--------------------------
type UserConfig = (Username, Password)
type Username = String
type Password = String

type URL = String

comunioLoginUrl :: UserConfig -> URL
comunioLoginUrl (username, pw) =
 "http://www.comunio.de/login.phtml?login="
   ++ username ++ "&pass=" ++ pw ++ "&action=login"

comunioLineupUrl :: URL
comunioLineupUrl = "http://www.comunio.de/lineup.phtml"

-------------------------------------
---------- HTML processing ----------
-------------------------------------

type XmlTreeValue a = a XmlTree String
type ParsedXmlTree a = a XmlTree XmlTree
type IOXmlTree = IOSArrow XmlTree XmlTree
type HtmlBody = String
type FileName = (String, String) -- (Name,Optional) f.e. ("gameday","24")
type TemplateFlag = Bool

-- selects and reconstructs table-structure of lineup for further processing
-- <players><player>...</player>...<player>...</player></players>
-- processLineup :: ArrowXml cat => ParsedXmlTree cat
processLineup = getXPathTrees $
       "/html/body//div[@id=\"center\"]//div[@id=\"content\"]" ++
       "//div[@id=\"smallcontentright\"]" ++
       "//div[@class=\"tablebox\"]//table" ++
       "//tr"

-- selects player and points from the lineup html-table
-- and constructs new XML structure
-- (f.e. <name>Beister</name><points>22</points>)
-- selectTableEntries :: ArrowXml cat => ParsedXmlTree cat
selectNames =
  processLineup >>> getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "left") >>> getChildren
selectPoints =
  processLineup >>> getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "right") >>> getChildren

-- parses a given HtmlBody with utf8-encoding
parseHtml :: HtmlBody -> IOStateArrow s b XmlTree
parseHtml htmlString = readString readOptions htmlString

makeHtmlFile :: FileName -> FilePath
makeHtmlFile (name,optional) = name++optional++".html"

makeTemplateFile :: FileName -> FilePath
makeTemplateFile (name,optional) = name++optional++".hamlet"

-- saveHtml :: TemplateFlag -> FileName -> String -> IOXmlTree
saveHtml True dst str = writeFile (makeTemplateFile dst) str


-- processHtml :: FileName -> HtmlBody -> IO [String]
processHtml dst htmlString = do
  names <- runX (parseHtml htmlString >>> textNodeToText selectNames)
  points <- runX (parseHtml htmlString >>> textNodeToText selectPoints)
  let list = zip names points
  let templateString = mkElemWithText "players" ("\n" ++ concatMap consTemp list)
  saveHtml True dst templateString
  return templateString
 where consTemp (a,b) = "  " ++ mkElem "player"
                      ++ "\n \t" ++ mkElemWithText "name" a
                      ++ "\n \t" ++ mkElemWithText "points" b ++ "\n"

-- Login via GET-Request, handle redirect and redirect to lineup page manually
-- lineUpContent :: UserConfig -> IO HtmlBody
lineUpContent userConfig = do
  (_,rsp) <- browse (
               setAllowRedirects True -- handle HTTP redirects
               >> (request $ getRequest (comunioLoginUrl userConfig)) -- login
               >> (request $ getRequest comunioLineupUrl)) -- redirect to lineup
  return (rspBody rsp)

-- all the above is triggered by this function
-- downloadHtml :: UserConfig -> FileName -> IO ()
downloadHtml userConfig dst = lineUpContent userConfig >>= processHtml dst

--------------------------------------
---------- Comparing Points ----------
--------------------------------------

type Points = Int

-- In order to search for a player name in an older or newer lineup,
-- we need both representations: BenchPlayer and Player
type BenchPlayer = String
type Player = String

-- removes stars in player names ("*Beister*" becomes "Beister")
removeStars :: Player -> BenchPlayer
removeStars pName =
  case pName of
   '*':name -> (delete '*' name)
   _ -> pName

-- adds stars in player names ("Beister" becomes "*Beister*")
addStars :: BenchPlayer -> Player
addStars pName =
  case head pName of
    '*' -> pName
    _ -> "*" ++ pName ++ "*"

-- neet function to select a value within a XmlTree as String (for debugging)
textNodeToText :: ArrowXml cat => ParsedXmlTree cat -> XmlTreeValue cat
textNodeToText selector = selector `when` isElem >>> getText

selectTextNode :: ArrowXml cat => ParsedXmlTree cat -> ParsedXmlTree cat
selectTextNode selector = selector `when` isElem

-- select all player names
-- (<players><player><name>A</name><points>2</points></player>
--           <player><name>B</name><points>2</points></player>
--  </players>
--   becomes [A,B])
player :: ArrowXml cat => ParsedXmlTree cat
player = getXPathTrees "/players/player/name/text()"

-- selector for player names (returns String)
playerNames :: ArrowXml cat => XmlTreeValue cat
playerNames = textNodeToText player

-- select the points for a given player
{-- (<players><player><name>pName</name><points>22</points></player></players>
 becomes 22) --}
pointsForName :: ArrowXml cat => String -> ParsedXmlTree cat
pointsForName pName =
  getXPathTrees (
    "/players/player[name='"++ pName++"']/points/text()"
    )

-- selector for a player's points (returns String)
pointsForPlayer :: ArrowXml cat => String -> XmlTreeValue cat
pointsForPlayer pName =
  textNodeToText
   (pointsForName pName <+> pointsForName (addStars pName))

-- returns points of a given player for two different data sources
getPointsForPlayer :: IOXmlTree -> IOXmlTree -> String -> IO (Points, Points)
getPointsForPlayer src1 src2 pName = do
  [currPoints] <- runX (src1 >>> pointsForPlayer pName)
  [oldPoints] <- runX (src2 >>> pointsForPlayer pName)
  return (read currPoints :: Points, read oldPoints :: Points)

calcDiff :: Points -> Points -> Points
calcDiff p1 p2 = p1 - p2

makeDiffHtml :: [(Int, String)] -> IOSArrow a XmlTree
makeDiffHtml pointsAndNames = root [] [mkelem "players" []
  (map (\(p,n) -> selem "player" [ selem "name"
       [txt (decode n)]
                                 , selem "diffPoints" [txt (show p)]])
   pointsAndNames)]
    >>> writeDocument writeOptions "diff-example.html"
 where decode = unicodeToLatin1 . fst . utf8ToUnicode

mkElem :: String -> String
mkElem str = "<" ++ str ++ ">"

mkElemWithText :: String -> String -> String
mkElemWithText str options = "<" ++ str ++ ">" ++ options


makeDiff :: [(Int,String)] -> String
makeDiff pointsAndNames =
  concatMap (\(p,n) -> mkElem  "player" ++ mkElemWithText "name" (decode n)
                                  ++ mkElemWithText "diffPoints" (show p)
                                  ++ "\n")
      pointsAndNames
 where decode = unicodeToLatin1 . fst . utf8ToUnicode

-- compDiff newSrc oldSrc = do
computeDiff :: IOXmlTree -> IOXmlTree -> IO ()
computeDiff newSrc oldSrc = do
  pNames <- runX (newSrc >>> playerNames)
  formattedPNames <- mapM (return . removeStars) pNames
  pList <- mapM (getPointsForPlayer newSrc oldSrc) formattedPNames
  diffList <- mapM (return . uncurry calcDiff) pList
  -- runX (makeDiffHtml (zip diffList pNames))
  writeFile "diff.hamlet" (makeDiff (zip diffList pNames))
  return ()