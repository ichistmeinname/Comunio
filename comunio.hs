import Network.Browser
import Network.HTTP
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Data.List (delete)

import Debug.Trace (trace)
import qualified Debug.Trace as D

---------- global read/wrote configs ----------
writeOptions :: [SysConfig]
writeOptions = [withIndent yes, withOutputEncoding utf8]

readOptions :: [SysConfig]
readOptions =  [withInputEncoding utf8, withParseHTML yes, withWarnings no]

--------- shortcut selectors for testing ----------
readSrc :: String -> IOXmlTree
readSrc filePath =
  readDocument readOptions filePath

old :: IOXmlTree
old = readSrc "old.html"

curr :: IOXmlTree
curr = readSrc "test.html"

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

-- selects and reconstructs table-structure of lineup for further processing
-- <players><player>...</player>...<player>...</player></players>
processLineup :: ArrowXml cat => ParsedXmlTree cat
processLineup =
 selem "players" $ [(getXPathTrees $
       "/html/body//div[@id=\"center\"]//div[@id=\"content\"]" ++
       "//div[@id=\"smallcontentright\"]" ++
       "//div[@class=\"tablebox\"]//table" ++
       "//tr") >>> selem "player" [selectTableEntries]]

-- selects player and points from the lineup html-table
-- and constructs new XML structure
-- (f.e. <name>Beister</name><points>22</points>)
selectTableEntries :: ArrowXml cat => ParsedXmlTree cat
selectTableEntries =
  selem "name" [(getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "left") >>> getChildren)]
  <+>
  selem "points" [(getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "right") >>> getChildren)]

extractLineUp :: IOXmlTree
extractLineUp = processChildren (processLineup `when` isElem)

-- parses a given HtmlBody with utf8-encoding
parseHtml :: HtmlBody -> IOStateArrow s b XmlTree
parseHtml htmlString = readString readOptions htmlString

-- saves a given XmlTree to destionation file with utf-encoding
-- in XML representation (<xml> ... </xml>)
saveHtml :: FilePath -> IOXmlTree
saveHtml dst = writeDocument [withOutputEncoding utf8, withIndent yes] dst

processHtml :: HtmlBody -> FilePath -> IO ()
processHtml htmlString dst =
  runX (
    parseHtml htmlString >>> extractLineUp >>> saveHtml dst)
  >> return ()

-- Login via GET-Request, handle redirect and redirect to lineup page manually
lineUpContent :: UserConfig -> IO HtmlBody
lineUpContent userConfig = do
  (_,rsp) <- browse (
               setAllowRedirects True -- handle HTTP redirects
               >> (request $ getRequest (comunioLoginUrl userConfig)) -- login
               >> (request $ getRequest comunioLineupUrl)) -- redirect to lineup
  return (rspBody rsp)

-- all the above is triggered by this function
downloadHtml :: UserConfig -> FilePath -> IO ()
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

computeDiff :: IOXmlTree -> IOXmlTree -> IO ()
computeDiff newSrc oldSrc = do
  pNames <- runX (newSrc >>> playerNames)
  formattedPNames <- mapM (return . removeStars) pNames
  pList <- mapM (getPointsForPlayer newSrc oldSrc) formattedPNames
  diffList <- mapM (return . uncurry calcDiff) pList
  runX (makeDiffHtml (zip diffList pNames))
  return ()

calcDiff :: Points -> Points -> Points
calcDiff p1 p2 = p2 - p1

makeDiffHtml :: [(Int, String)] -> IOSArrow a XmlTree
makeDiffHtml pointsAndNames = root [] [mkelem "players" []
  (map (\(p,n) -> selem "player" [ selem "name" [txt n]
                                 , selem "diffPoints" [txt (show p)]])
   pointsAndNames)]
    >>> writeDocument writeOptions "diff-example.html"