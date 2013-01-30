import Network.Browser
import Network.HTTP
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Text.XML.HXT.Curl
import Data.List (delete)
import Data.Tree.NTree.TypeDefs (NTree)

import Debug.Trace (trace)
import qualified Debug.Trace as D

type Points = Int
type URI = String

comunioLoginPage :: String -> String -> URI
comunioLoginPage username pw =
 "http://www.comunio.de/login.phtml?login="
   ++ username ++ "&pass=" ++ pw ++ "&action=login"

comunioLineup :: URI
comunioLineup = "http://www.comunio.de/lineup.phtml"

lineUpContent :: String -> String -> IO String
lineUpContent username pw = do
  (_,rsp) <- browse (
               setAllowRedirects True -- handle HTTP redirects
               >> (request $ getRequest (comunioLoginPage username pw)) -- login
               >> (request $ getRequest comunioLineup)) -- redirect to lineup
  return (rspBody rsp)

test :: String -> String -> FilePath -> IO ()
test username pw dst = lineUpContent username pw >>= processHtml dst

processHtml htmlString dst =
  runX (
    parseHtml htmlString >>> extractLineUp >>> saveHtml dst)
  >> return ()

parseHtml :: String -> IOStateArrow s b XmlTree
parseHtml htmlString = readString
          [ withInputEncoding utf8
          , withParseHTML yes
          , withCurl []
          ] htmlString

extractLineUp :: IOSArrow (NTree XNode) (NTree XNode)
extractLineUp = processChildren (processLineup `when` isElem)

saveHtml :: FilePath -> IOStateArrow s XmlTree XmlTree
saveHtml dst = writeDocument [withOutputEncoding utf8] dst

processLineup :: ArrowXml a => a XmlTree XmlTree
processLineup =
 selem "players" $ [(getXPathTrees $
       "/html/body//div[@id=\"center\"]//div[@id=\"content\"]" ++
       "//div[@id=\"smallcontentright\"]" ++
       "//div[@class=\"tablebox\"]//table" ++
       "//tr") >>> selem "player" [selectTableEntries]]

selectTableEntries :: ArrowXml a => a XmlTree XmlTree
selectTableEntries =
  selem "name" [(getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "left") >>> getChildren)]
  <+>
  selem "points" [(getChildren >>> hasName "td" >>>
    hasAttrValue "align" (== "right") >>> getChildren)]

readSrc :: String -> IOStateArrow s b XmlTree
readSrc filePath = readDocument [withTrace 0
                , withInputEncoding utf8, withWarnings no] filePath

computeDiff :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
            -> IO [(Int, Int)]
computeDiff newSrc oldSrc = do
  players <- runX (newSrc >>> playerNames)
  mapM (comparePlayers newSrc oldSrc) (map removeStars players)

comparePlayers :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
               -> String -> IO (Points, Points)
comparePlayers src1 src2 pName = do
  [currPoints] <- runX (src1 >>> pointsForPlayer pName)
  [oldPoints] <- runX (src2 >>> pointsForPlayer pName)
  return (read currPoints :: Int, read oldPoints :: Int)

treeToText :: ArrowXml cat => cat XmlTree XmlTree -> cat XmlTree String
treeToText selector = selector `when` isElem >>> getText

pointsForPlayer :: ArrowXml cat => String -> cat XmlTree String
pointsForPlayer name =
  treeToText
   (pointsForName name <+> pointsForName ("*"++ name ++"*"))

playerNames :: ArrowXml cat => cat XmlTree String
playerNames = treeToText player

pointsForName :: ArrowXml a => String -> a XmlTree XmlTree
pointsForName name =
      (getXPathTrees (
     "/players/player[name='"++ name
      ++"']/points/text()"))

player :: ArrowXml a => a XmlTree XmlTree
player = getXPathTrees "/players/player/name/text()"

old :: IOStateArrow s b XmlTree
old = readSrc "old.html"

curr :: IOStateArrow s b XmlTree
curr = readSrc "test.html"

removeStars :: String -> String
removeStars pName =
  case pName of
   '*':name -> delete '*' name
   _ -> pName