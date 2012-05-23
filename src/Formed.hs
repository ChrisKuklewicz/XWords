-- Try to hide most of the form validation logic.
-- 'validate' takes the values returned by Network.CGI.getInput and sanity checks it
-- and converts it either a dire error message or a ValidateResult which
-- might contain a valid ScrabbleCommand and also contains per field values and error messages
module Formed (fieldNames,field_hand,field_template,field_whole,field_sort,field_reverse,field_submit
              ,field_sort_range,field_checkbox_value,field_submit_Lookup,field_submit_Template
              ,validate,getCommand,getInputValue,getInputBool,getInputErrs,getValidInputs
              ,ScrabbleCommand(..),Sort(..),Validity(..)
              ,InputName(unInputName),InputValue(..),InputErrorMessage(..),ValidateResult) where

import Control.Monad(when,liftM)
--import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Data.Char(isAlphaNum,isSpace)
import Data.Ix(inRange)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Monoid(Monoid(..))
import qualified Data.Set as S
import Numeric(readDec)

import CERWS
import Scrabble(ScrabbleCommand(..),Sort(..))

-- Type of strings used as input names. The InputName constructor is
-- not exported from Formed.
newtype InputName = InputName { unInputName :: String } deriving (Ord,Eq,Show)

inputName :: String -> InputName
inputName s | validInputName s = InputName s
            | otherwise = error ("Formed: Cannot make non AlphaNum InputName ("++s++")")

validInputName :: String -> Bool
validInputName "" = False
validInputName xs = all isAlphaNum xs

-- Exported symbols holding the field names, other modules cannot
-- construct any other InputName's

field_hand,field_template,field_whole,field_sort,field_reverse,field_submit :: InputName
field_hand     = inputName "line"
field_template = inputName "template"
field_whole    = inputName "whole"
field_sort     = inputName "sort"
field_reverse  = inputName "reverse"
field_submit   = inputName "submit"

fieldNames :: S.Set InputName
fieldNames = S.fromList [field_hand,field_template,field_whole,field_sort,field_reverse,field_submit]

-- Value used for all checkbox controls

field_checkbox_value :: String
field_checkbox_value = "1"

-- Exported range of field_sort

field_sort_range :: (Int,Int)
field_sort_range = (1,3)

-- Exported symbols describing valid values to field_submit

field_submit_Lookup,field_submit_Template :: String
field_submit_Lookup   = "Lookup"
field_submit_Template = "Template"

field_submit_values :: [String]
field_submit_values = [field_submit_Lookup, field_submit_Template]

-- A variant of Map that exploits the monoid structure of the values

newtype (Monoid v) => MonoidMap k v = MonoidMap {unMonoidMap :: (M.Map k v)} deriving Show
instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap M.empty
  mappend (MonoidMap x) (MonoidMap y) = MonoidMap (M.unionWith mappend x y)

-- Explicitly model the concept of states of validation as a Monoid
data Validity = Unknown | Valid | Invalid deriving (Eq,Show)

instance Monoid Validity where
  mempty = Unknown
  mappend Unknown x = x
  mappend x Unknown = x
  mappend Invalid _ = Invalid
  mappend _ Invalid = Invalid
  mappend Valid Valid = Valid

data InputValue = InputValue { ivOk :: Validity, ivToList :: [String] }
  deriving (Show)

instance Monoid InputValue where
  mempty = InputValue { ivOk = mempty, ivToList = mempty }
  mappend a b = InputValue { ivOk = ivOk a `mappend` ivOk b
                           , ivToList = ivToList a `mappend` ivToList b
                           }

badIV :: InputValue
badIV = InputValue { ivOk = Invalid, ivToList = mempty }

newtype InputErrorMessage = InputErrorMessage { unInputErrorMessage :: String }
  deriving Show

-- The type of an at least partially successful validation is ValidateResult

type ValidateResult = (Maybe ScrabbleCommand
                      ,Map InputName [InputErrorMessage]
                      ,Map InputName InputValue)

getCommand :: ValidateResult -> Maybe ScrabbleCommand
getCommand (x,_,_) = x

getInputValue :: ValidateResult -> InputName -> (Validity,String)
getInputValue (_,_,x) n = case M.lookup n x of
                            Just (InputValue {ivOk=ok,ivToList=[v]}) -> (ok,v)
                            _ -> (Invalid,"")

getInputBool :: ValidateResult -> InputName -> (Validity,Bool)
getInputBool (_,_,x) n = case M.lookup n x of
                           Just iv@(InputValue {ivToList=[]}) -> (ivOk iv,False)
                           Just iv@(InputValue {ivToList=[_]}) -> (ivOk iv,True)
                           _ -> (Invalid,False)

getValidInputs :: ValidateResult -> [(String,String)]
getValidInputs (_,_,x) = concatMap toStrings  $ M.assocs x
  where toStrings (InputName a,InputValue {ivOk=Valid,ivToList=bs}) = map ((,) a) bs
        toStrings _ = []

getInputErrs :: ValidateResult -> InputName -> [InputErrorMessage]
getInputErrs (_,x,_) n = maybe [] id (M.lookup n x)

-- Most of validation is done in the 'W' monad; This is very much like
-- ErrorT String (WriterT (MonoidMap InputName [InputErrorMessage]) (State MonoidMap InputName InputValue)
-- but implemented with an additional ContT layer and flattened into pure continuation passing style in CERWS.hs
-- (the MonadCont and MonadReader features of CERWS are currently unused)
-- The final type of runCERWS on (W a) is then either a dire error or
-- Error String (a,MonoidMap InputName [InputErrorMessage],MonoidMap InputName InputValue)

type W a = ERWS String () (MonoidMap InputName [InputErrorMessage]) (MonoidMap InputName InputValue) a

-- W ops, which are the layer above the MonadError, MonadWriter, and MonadState of ERWS.

complain :: InputName -> String -> W ()
complain i e = tell (MonoidMap (M.singleton i [InputErrorMessage e]))

appendField :: InputName -> InputValue -> W ()
appendField k v = do
  mOld <- get
  let mNew = mAppend k v mOld
  mNew `seq` put $! mNew
 where
  mAppend k1 v1 (MonoidMap m) = MonoidMap (M.insertWith' mappend k1 v1 m)

putField :: InputName -> InputValue -> W ()
putField k iv = do
  MonoidMap mOld <- get
  let mNew = M.insert k iv mOld
  mNew `seq` put $! MonoidMap mNew

getField :: InputName -> W (Maybe InputValue)
getField k = liftM (M.lookup k . unMonoidMap ) get

getValidField :: InputName -> W (Maybe String)
getValidField k = liftM (ifValidSingle . M.lookup k . unMonoidMap ) get
  where ifValidSingle (Just (InputValue {ivOk = Valid, ivToList = [x]})) = Just x
        ifValidSingle _ = Nothing

getValidCheckbox :: InputName -> W (Maybe Bool)
getValidCheckbox k = liftM (ifValidCB . M.lookup k . unMonoidMap ) get
  where ifValidCB (Just (InputValue {ivOk = Valid, ivToList = [_]})) = Just True
        ifValidCB (Just (InputValue {ivOk = Valid, ivToList = []}))  = Just False
        ifValidCB _ = Nothing

die :: String -> W a
die s = throwError (header ++ s ++ ")")
  where header = "Formed.hs validation error : ("

-- Combine the untrusted inputs into the inputMap, apply the
-- individual validation functions, and pass through crossValidate.
validate :: [(String,String)] -> Either String ValidateResult
validate inputs = unMM $ runCERWS () emptyValues $ do
  collectInputs inputs
  crossValidate
 where unMM (Right (a,w,s)) = Right (a,unMonoidMap w,unMonoidMap s)
       unMM (Left e) = Left e
       emptyValues = MonoidMap ( M.fromDistinctAscList . map (\i -> (i,mempty)) . S.toAscList $ fieldNames )

-- The input list of (String,String) is untrusted.  The MonoidMap
-- already has the expected InputName keys.  The keys are allowed to
-- have multiple values, so the new and old values are combined.
-- Reporting errors with the untrested input is tricky, use the
-- toTrusted function to limit the reported name to the first 80
-- characters.
collectInputs :: [(String,String)] -> W ()
collectInputs ivs = mapM_ collectOne ivs where
  collectOne (k,v) | validInputName k && S.member inK fieldNames =
    appendField inK (InputValue { ivOk=Unknown, ivToList=[v] })
                   | otherwise = die ("Unexpected input field name: "++take 16 k)
   where inK = InputName k

-- The crossValidate function handles the final validation by checking
-- the values of the inputs with respect to each other, i.e. depending
-- on which submit button was pressed.  If all is okay, then produce a
-- ScrabbleCommand.
crossValidate :: W (Maybe ScrabbleCommand)
crossValidate = do
  validator -- check the individual fields
  m_val_sumit <- getValidField field_submit
  case m_val_sumit of
    (Just v) | v==field_submit_Lookup -> validLookup
             | v==field_submit_Template -> validTemplate
             | otherwise -> die "Unrecognized command in crossValidate"
    _ -> return Nothing
 where
  makeSort = do (Just n) <- getValidField field_sort
                (Just r) <- getValidCheckbox field_reverse
                return (Sort {iMethod=read n,bReversed=r})

  validLookup = do
    s <- makeSort
    maybe'hand <- getValidField field_hand
    case maybe'hand of
      (Just hand) -> return (Just (LookupCommand {sTiles = hand
                                                 ,getSort = s}))
      _ -> return Nothing

  validTemplate = do
    s <- makeSort
    maybe'hand <- getValidField field_hand
    maybe'template <- getField field_template
    case (maybe'hand,maybe'template) of
      (Just hand, Just (InputValue {ivOk=Valid,ivToList=[template]})) -> do
        (Just whole) <- getValidCheckbox field_whole
        return . Just $
          if whole then TemplateCommand  {sTiles = hand, sTemplate = template, getSort = s}
                   else TemplatesCommand {sTiles = hand, sTemplate = template, getSort = s}

      (Just hand, Just iv) | null (ivToList iv) || [""]==ivToList iv -> do
        complain field_template "Your template was blank, so just a Lookup being performed"
        return (Just (LookupCommand {sTiles = hand
                                    ,getSort = s}))
      _ -> return Nothing

validator :: W ()
validator = sequence_ [ field_hand_valid
                      , field_template_valid
                      , field_whole_valid
                      , field_sort_valid
                      , field_reverse_valid
                      , field_submit_valid
                      ]
  where
    validCheckbox :: InputName -> W ()
    validCheckbox i = do
      (MonoidMap m) <- get
      case M.lookup i m of
        Just iv -> case ivToList iv of
                     [v] | v == field_checkbox_value -> putField i (iv {ivOk=Valid})
                     []   -> putField i (iv {ivOk=Valid})
                     _ -> die ("Field "++unInputName i++" has unexpected value(s)")
        Nothing -> do
          die ("Field "++unInputName i++" is unexpected and cannot be validated\n")

    -- wantSingle helps make functions for input fields that do not
    -- accept multiple InputValues.
    withSingle :: InputName -> (String -> W ()) -> W ()
    withSingle i f = do
      (MonoidMap m) <- get
      case M.lookup i m of
        Just iv -> case ivToList iv of
                     [v] -> f v
                     []  -> putField i badIV
                     _   -> die ("Field "++unInputName i++" has multiple values")
        Nothing ->
          die ("Field "++unInputName i++" is unexpected and cannot be validated\n")


    field_hand_valid = withSingle field_hand $ \handIn -> do
      if null handIn
        then do complain field_hand "Cannot search with empty hand"
                putField field_hand badIV
        else do let (clean,lengthOk,isUsable) = washHand handIn
                    ok | isUsable && not (null clean) = Valid
                       | otherwise = Invalid
                when (not lengthOk) $ complain field_hand
                  "Input was too long, trimmed to 4 dots and 30 other characters (spaces are stripped)"
                when (not isUsable) $ complain field_hand
                  "Invalid characters in hand, use only '.' or a-z or A-Z (spaces are stripped)"
                when (isUsable && null clean) $ complain field_hand
                  -- only spaces were in the input field
                  "Cannot search with empty hand (spaces were stripped)"
                putField field_hand (InputValue { ivOk=ok, ivToList=[clean] })

    -- be forgiving about blank space
    -- take first 30 non '.' characters
    -- take first 4 '.' characters
    -- the textOut2 is clean enough to allow it to be returned to the user
    washHand line = (textOut2,lengthOk,isUsable) where
      textIn = filter (not .isSpace) line
      textOut1 = go 30 4 textIn
      textOut2 = go 30 4 . filter (`S.member` validSet) $ textIn
      validSet = S.fromList ('.':['a'..'z']++['A'..'Z'])
      lengthOk = length textOut1 == length textIn
      isUsable = textOut1 == textOut2
      -- go is a specialized variant of take, its virtue is keeping the input in the proper order
      go :: Int -> Int -> String -> String
      go _ _ [] = []
      go 0 0 _ = []
      go n 0 ('.':xs) = go n 0 xs
      go n w ('.':xs) = '.':go n (pred w) xs
      go 0 w (_:xs) = go 0 w xs
      go n w (x:xs) = x:go (pred n) w xs

    field_template_valid = withSingle field_template $ \templateIn -> do
      if null templateIn then putField field_template badIV
        else do let (clean,lengthOk,isUsable) = washTemplate templateIn
                    ok | isUsable = Valid
                       | otherwise = Invalid
                when (not lengthOk) $ complain field_template
                  "Input was too long, trimmed to first 15 characters (leading and trailing spaces stripped)"
                when (not isUsable) $ complain field_template
                  "Invalid characters in template, use only '-' or 2-5 or a-z or A-Z"
                putField field_template (InputValue { ivOk=ok, ivToList=[clean] })

    -- be forgiving about blank space at beginning and end
    -- take first 15 characters, the width of scrabble board
    -- the textOut2 is clean enough to allow it to be returned to the user
    washTemplate line = (textOut2,lengthOk,isUsable) where
      textIn = reverse . dropWhile isSpace . reverse . dropWhile isSpace $ line
      textOut1 = take 15 textIn
      textOut2 = filter (`S.member` validSet) $ take 15 textIn
      validSet = S.fromList ('-':['2'..'5']++['a'..'z']++['A'..'Z'])
      lengthOk = length textOut1 == length textIn
      isUsable = textOut1 == textOut2

    field_whole_valid = validCheckbox field_whole

    field_sort_valid = withSingle field_sort $ \sortIn -> do
      let maxLength = length (show (snd field_sort_range)) -- short enough now I can ignore Int overflow in readDec
      case splitAt maxLength sortIn of
        (front,"") -> case readDec front of
                       [(n,"")] | inRange field_sort_range n && show n == sortIn ->
                                    putField field_sort (InputValue { ivOk = Valid, ivToList=[sortIn] })
                       _ -> die ("Invalid value in "++unInputName field_sort)
        _ -> die ("Too long a value given for "++unInputName field_sort)

    field_reverse_valid = validCheckbox field_reverse

    field_submit_valid = withSingle field_submit $ \submitIn -> do
      when (submitIn `notElem` field_submit_values) $
        die ("Unexpected value sent for "++ unInputName field_submit)
      putField field_submit (InputValue { ivOk = Valid, ivToList=[submitIn] })
