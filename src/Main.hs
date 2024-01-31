module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
--import qualified Data.Sequence as DB

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  let newDB = DB.empty
  result <- DB.save newDB
  case result of 
    Success _ -> return ()
    Error _ -> putStrLn "Error creating database"

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  dbResult <- DB.load
  case dbResult of 
    Error _ -> putStrLn "Failed to load DB"
    Success db -> 
      case DB.findFirst (\findEntry -> entryId findEntry == getOptId getOpts) db of
        Just findEntry -> putStrLn (entrySnippet findEntry)
        Nothing -> putStrLn "Found nothing " 
      

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  dbResult <- DB.load
  case dbResult of 
    Error _ -> putStrLn "Failed to load DB"
    Success db ->
      let matchingEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db
      in
        if null matchingEntries
          then putStrLn "No entries found"
          else mapM_ (putStrLn . show . FmtEntry) matchingEntries

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =  do
  dbResult <- DB.load
  case dbResult of
    Error _ -> putStrLn "Failed to load DB"
    Success db -> do
      let snippetFile = addOptFilename addOpts
      snippetContent <- readFile snippetFile
      let existingEntry = DB.findFirst (\entry -> entrySnippet entry == snippetContent) db
      case existingEntry of
        Just found -> putStrLn  ("Entry with this content already exists: \n" ++ show (FmtEntry found ))
        Nothing -> do
          let newId = maybe 0 (\entry -> entryId entry + 1) (DB.findFirst (const True) db)
          let newEntry = makeEntry newId snippetContent addOpts
          let modifiedDB = DB.insertWith (const newEntry) db
          saveResult <- DB.save modifiedDB
          case saveResult of
            Error _ -> putStrLn "Failed to save DB"
            Success _ -> putStrLn "Entry added successfully"
  
  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
