-- Ownership Info

-- Name:   Anthony Bobsin
-- Course: CPS506, Winter 2016, Assignment #4
-- Due:    2016.04.15 08:30
-- Credit: This is entirely my own work.

-- Rationale: The entry point to this executable is the startOn function.
-- The startOn function receives two parameters. The first being the URL
-- - as a String - for which you want to begin crawling. The second being an
-- empty Array, this will be used for passing options to the crawler when
-- implemented. Inside the startOn function, we attempt to parse the URL to
-- see if the URL received is valid. If not valid, we print an error message and
-- return. If valid, we call the crawl function on the URL.
--
-- The crawl function utilizes the http-client library to send a GET request to
-- the URL passed. After receiving the response, we send the body to the parseHTML
-- function.
--
-- The parseHTML function splits up the body by new lines, and passes each line to both
-- the parseLineForTags and parseLineForLinks functions. The parseLineForTags function
-- uses regular expressions to find tags within the line, and returns an array of all tags
-- found. The parseLineForLinks function does the same but for links. With the array of
-- tags received, we call uniqueTagCount which creates a Data.Map based on the unique tags found
-- and the number of each in the non-unique tags array. We then print the tagCount found,
-- and call crawl on all links found from this body.

module Assignment4 () where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

startOn :: String -> [String] -> IO()
startOn url [] = case parseURI url of
  Nothing -> putStrLn "Invalid URL given"
  Just u -> crawl url

crawl :: String -> IO()
crawl url = do
  manager <- newManager defaultManagerSettings

  request <- parseUrl url
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  parseHTML $ show $ responseBody response

parseHTML :: String -> IO()
parseHTML body = do
  let tags = Prelude.filter (not . Prelude.null) (concat (Prelude.map parseLineForTags $ splitOn "\\n" body))
  let links = Prelude.filter (not . Prelude.null) (concat (Prelude.map parseLineForLinks $ splitOn "\\n" body))
  let uniqueTags = Data.List.nub tags
  let tagCount = uniqueTagCount uniqueTags tags

  print tagCount
  mapM_ crawl links

uniqueTagCount :: [String] -> [String] -> Map String String
uniqueTagCount uniqueTags tags = Map.fromList(map uTagCount uniqueTags)
  where uTagCount tag = (tag, show (length (filter (==tag) tags)))

parseLineForLinks :: String -> [String]
parseLineForLinks line = do
  let (_, tag, tagContents) = line =~ "< ?([A-Za-z]+)" :: (String,String,String)

  if not (tag == "" && tagContents == "")
    then do
      Prelude.map last (line =~ "href=\"(http:[^ ]+)\"" :: [[String]])
    else return []

parseLineForTags :: String -> [String]
parseLineForTags line = do
  let (_, tag, tagContents) = line =~ "< ?([A-Za-z]+)" :: (String,String,String)

  if not (tag == "" && tagContents == "")
    then do
      Prelude.map last (line =~ "< ?([A-Za-z]+)" :: [[String]])
    else return []
