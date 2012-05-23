module Main(main) where

import Paths_xwords
-- External libraries
import Control.Concurrent(forkIO)
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TVar(readTVar,writeTVar,TVar)
import Control.OldException(Exception(AssertionFailed))
import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Error.Class(MonadError(throwError),Error(strMsg))
import Data.List(deleteBy,sort)
import Network.FastCGI(runFastCGIConcurrent,getInputs,requestURI
                      ,formEncode,output,outputException,handleErrors
                      ,CGI,CGIResult)
import Network.URI(URI(uriQuery))
import System.Environment(getArgs)
import Text.XHtml(prettyHtml,stringToHtml,hotlink,toHtml)

-- Internal libraries
import Formed(validate,field_submit,getCommand,getValidInputs
             ,InputName(unInputName),ValidateResult)
import GenPage(makePage)
import Scrabble(makeDict,process,prettySC,Dict)
import StateKey(newSessions,GetClient,Client(state),History(history),HIST)
import Dict(seqHack)

main :: IO ()
main = do
  -- Create in-memory global client tracker (and persistent background cache cleaning thead)
  getClient <- newSessions
  -- Load dictionary from disk (and one-shot background thread to force the lazy read)
  dict <- loadDict
  -- Start handling requests (with a up to 10 threads)
  runFastCGIConcurrent 10 (handleErrors (cgiMain getClient dict))

loadDict :: IO Dict
loadDict = do
  args <- getArgs
  defaultFile <- getDataFileName "words"
  file <- case args of
            [] -> return defaultFile
            [fileIn] -> return fileIn
            _ -> die ("Single argument of the dictionary file name required (or no argument for "++defaultFile++")")
  attempt'load <- makeDict file
  dict <- case attempt'load of
         Left err -> die ("Could not load the file, with error (" ++ err ++ ")")
         Right trie -> return trie
  forkIO (seqHack dict `seq` return ())  -- force it to load dictionary in a background thread
  return dict

-- This is the main action of the server which is done once for each
-- client http request.
cgiMain :: GetClient -> Dict -> CGI CGIResult
cgiMain getClient dict = do
  -- Lookup the client's history (or create a new one)
  (history1,update) <- accessHistory =<< getClient
  inputsRaw <- getInputs -- The identity and contents of the url's form parameters
  -- Check for any serious error in the url's parameters
  uri <- requestURI
  case validate inputsRaw of
    Left err -> outputErr inputsRaw err
    Right valid ->
      -- Check for well formed search command from the url's parameters
      case getCommand valid of
        Nothing -> output . prettyHtml $ makePage uri valid (Left "No Input") history1
        Just command -> do
          let result = process dict command
              bestScore = if null result then 0 else maximum (map fst result)
          -- Create a new entry in the client's history (and store it in the cache)
          history2 <- update $ getHistLink valid (prettySC command bestScore) uri
          -- logCGI ("[scrabble fastcgi] "++show command)
          output . prettyHtml $ makePage uri valid (Right result) history2

-- 'getHistLink' takes the string and constructs a HIST to put into
-- the cached history, in this case it is the input String paired with
-- an Html fragment which is a link, using the String as the text,
-- which will reload the current page with the current value of the
-- text fields and controls (but not also cause it to be submitted and
-- the search re-run).
getHistLink :: ValidateResult -> String -> URI -> HIST
getHistLink valid pretty thisUri =
  let values' = sort . filter ((unInputName field_submit /=) . fst) . getValidInputs $ valid
      queryPart = '?':formEncode values'
      url = show (thisUri { uriQuery = queryPart })
  in (pretty, toHtml (hotlink url (stringToHtml pretty)))

-- In the event of a serious validation error, this will report a
-- '500' error and the message along with a truncated report of the
-- input.
outputErr :: [(String,String)] -> String -> CGI CGIResult
outputErr inputsRaw err =
  outputException . AssertionFailed . unlines $ 
    ["Validation error occurred: ",err," initial form parameters were "]
    ++(map show . map (\(a,b) -> (take 20 a,take 20 b)) . take 20 $ inputsRaw)

-- Take what may be a cached History and perhaps return a pair of the
-- contents of the history and an operation to update that history
accessHistory :: Either String (a,Client History)
              -> CGI ( Either String [HIST]
                     , HIST -> CGI (Either String [HIST])
                     )
accessHistory (Left str) =
  return (noCookiesMsg str, const (return (noCookiesMsg str)))
 where
  noCookiesMsg s = Left ("Cannot record history (no browser cookies?)\n"++s)

accessHistory (Right (_,client)) = do
  histLines <- liftIO . atomically $ readTVar tvHist
  return (Right histLines,updateHist)
 where
  tvHist :: TVar [HIST]
  tvHist = history (state client)

  -- This is a method to prepend a new HIST the cached [HIST]
  updateHist :: (MonadIO  m) => HIST -> m (Either String [HIST])
  updateHist line = liftIO . atomically $ do
    old <- readTVar tvHist
    let new = line : deleteBy ((==) `on` fst) line old
    writeTVar tvHist new
    return (Right new)

on :: (b->b->c) -> (a->b) -> (a -> a -> c)
f `on` g = (\a b -> (g a) `f` (g b))

die :: (MonadIO m) => String -> m a
die msg = liftIO . throwError . strMsg $ "Scrabble Main module died.\nError was: ("++msg++")"
