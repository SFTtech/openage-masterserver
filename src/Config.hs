-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Config where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec

import qualified Data.Map as Map


-- the config file has these entries
data Config = Config {
  netPort    :: Int,
  dbHost     :: String,
  dbName     :: String,
  dbUser     :: String,
  dbPassword :: String
} deriving (Show)

-- convert the string -> string map to our wanted config
createConfig :: StringMap -> Maybe Config
createConfig m = do
  port <- Map.lookup "port" m
  dbhost <- Map.lookup "db_host" m
  dbname <- Map.lookup "db_name" m
  dbuser <- Map.lookup "db_user" m
  dbpass <- Map.lookup "db_password" m
  Just (Config (read port) dbhost dbname dbuser dbpass)


type StringMap = Map.Map String String

-- config key parser
identifier :: Parser String
identifier = do
  c <- letter
  cs <- many (letter <|> digit <|> char '_')
  return (c:cs)
  <?> "identifier"

-- comment parser
comment :: Parser ()
comment = char '#' >> skipMany (noneOf "\n") <?> "comment"


-- end of line parser
eol :: Parser ()
eol = oneOf "\n" >> return () <?> "end of line"

-- single line parser
item :: Parser (String, String)
item = do
  key <- identifier     -- config entry key
  skipMany space
  char '='         -- aand assign a value to that key
  skipMany space
  value <- manyTill anyChar (try eol <|> try comment <|> eof)
  return (key, rstrip value)
  where rstrip = reverse . dropWhile isSpace . reverse


-- line parser, is nothing if line is a comment only
line :: Parser (Maybe (String, String))
line = do
  skipMany space
  try (comment >> return Nothing) <|> (item >>= return . Just)

-- file parser, contains many lines
file :: Parser [(String, String)]
file = do
  ls <- many line        -- read all the lines in the file
  return (catMaybes ls)  -- drop all nothings

readConfigMap :: SourceName -> IO (Either ParseError StringMap)
readConfigMap name = do
  result <- parseFromFile file name
  return (case result of
           Left err -> Left err  -- reversed to overwrite older entries:
           Right xs -> Right (Map.fromList (reverse xs)))

readConfig :: FilePath -> IO Config
readConfig path =
  do
    cmap <- readConfigMap path
    case cmap of
     Left err -> error ("config parsing failed: " ++ (show err))
     Right m  -> do
       let cfg = createConfig m
       case cfg of
        Nothing -> error "config file has missing keys"
        Just c -> do
          putStrLn ("configuration:\n" ++ (show c))
          return c
