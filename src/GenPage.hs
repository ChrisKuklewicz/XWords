module GenPage(makePage) where

import Data.List(intersperse)
import Text.Printf(printf)
import Text.XHtml
import Network.URI

import CSS(myCSS)
import Formed(field_hand,field_template,field_whole,field_sort,field_reverse,field_submit
             ,field_checkbox_value,field_submit_Lookup,field_submit_Template
             ,getInputValue,getInputBool,getInputErrs
             ,InputName(unInputName),Validity(Valid)
             ,InputErrorMessage(unInputErrorMessage),ValidateResult)
import Scrabble(Score)
import StateKey(HIST)

makePage :: URI -> ValidateResult -> Either String [(Score,String)] -> Either String [HIST] -> Html
makePage uri valid result hist = top +++ body << contents where
  top = header << (thetitle << stringToHtml "Scrabble™ Dictionary Search" +++ css)
  pageName = uriToString id (uri { uriQuery = "", uriFragment = "" }) ""
  contents = pageForm pageName valid +++ divMain result +++ divHist hist

  css :: Html
  css = style ! [ thetype "text/css" ] << primHtml myCSS
  withBorder = thediv ! [ theclass "border" ]
  divMain x = (thediv ! [ identifier "results", theclass "left"  ]) << withBorder << (heading +++ p ! [ theclass "tt" ] << (formatResults x))
    where heading = h3 ! [ theclass "center" ] << stringToHtml "Results"
  divHist x = (thediv ! [ identifier "history", theclass "right" ]) << withBorder << (heading +++ p << (formatHist x))
    where heading = h3 ! [ theclass "center" ] << stringToHtml "History"

  formatResults ::Either String [(Score,String)] -> Html
  formatResults (Left msg) = stringToHtml msg
  formatResults (Right []) = stringToHtml "No Results"
  formatResults (Right results) = linesToHtml $ map (\(i,s) -> printf "%3d %s\n" i s) results

  formatHist :: Either String [HIST] -> Html
  formatHist (Left msg) = stringToHtml msg
  formatHist (Right []) = stringToHtml "No History"
  formatHist (Right xs) = concatHtml . intersperse br . map snd $ xs


pageForm :: String -> ValidateResult -> Html
pageForm pageName valid = h2 ! [ theclass "center" ] << stringToHtml "Enter the query" +++ inputForm where
  inputForm = form ! [action pageName, method "get" ] << fieldset << concatHtml
    [legend << stringToHtml "Enter your tiles and template"
    ,makeField "Tiles in your hand (case insensitive, use '.' for wild) :" (textInput 45) field_hand
    ,p << linesToHtml ["The 50 point bonus to the score is awarded if and only if there are 7 tiles in the hand."
                      ,"The search is limited to use only up to 4 wild and 30 letter tiles."
                      ,"Hands that cannot occur in the game are limited to their top 100 results."
                      ]
    ,makeField "Template to use:" (textInput 45) field_template
    ,p << linesToHtml ["- for blank spaces"
                      ,"2 for blank double letter"
                      ,"3 for blank triple letter"
                      ,"4 for blank double word"
                      ,"5 for blank triple word"
                      ,"A-Z for existing normal tiles"
                      ,"a-z for existing wild tiles"
                      ]
    ,makeCheckbox field_whole "Check this to match only the whole template"
    ,p << (stringToHtml "Sort by: " +++ concatHtml (makeRadiosLabelRight field_sort
                      ["Score","Length","Alphabetical"]))
    ,makeCheckbox field_reverse "Check this to reverse the search"
    ,submit (unInputName field_submit) field_submit_Template
    ,submit (unInputName field_submit) field_submit_Lookup
    ,reset "Reset" "Reset"
    ]

  makeCheckbox :: InputName -> String -> Html
  makeCheckbox inputName promptS =
    let errs = getInputErrs valid inputName
        errHeader | null errs = noHtml
                  | otherwise = p . linesToHtml . map unInputErrorMessage $ errs
        state | snd ( getInputBool valid inputName ) = [checked]
              | otherwise = []
    in errHeader +++ p << (checkbox (unInputName inputName) field_checkbox_value ! state
                          +++ label ! [thefor (unInputName inputName)] << stringToHtml promptS)

  -- This takes a (string to show on page as the prompt)
  --              (function that takes the input label and produces an input widget)
  --              (the input label to use)
  --   From the pageForm scope, this uses the valid :: ValidateResult value
  makeField :: String -> (String -> Html) -> InputName -> Html
  makeField promptS kind inputName =
      let errs = getInputErrs valid inputName
          errHeader | null errs = noHtml
                    | otherwise = p . linesToHtml . map unInputErrorMessage $ errs
          addValue x = let v = snd (getInputValue valid inputName)
                       in x ! [value v]
      in errHeader +++ p << (label ! [thefor (unInputName inputName)] << stringToHtml promptS
                             +++ addValue (kind ! [ identifier (unInputName inputName)] $ (unInputName inputName)))

  -- Radio button have values of "1", "2", "3", etc.
  makeRadiosLabelRight :: InputName -> [String] -> [Html]
  makeRadiosLabelRight inputName xs = map makeRadio (zip (map show [defOne..]) xs) where
    defOne :: Int
    defOne = 1
    isDefault = case getInputValue valid inputName of
                  (Valid,def) -> (def==)
                  _ -> (show defOne==)
    makeRadio (v,promptS) | isDefault v =
        label << (radio (unInputName inputName) v ! [checked] +++ stringToHtml promptS)
                          | otherwise = 
        label << (radio (unInputName inputName) v +++ stringToHtml promptS)

  textInput :: Int -> (String -> Html)
  textInput c | c <= 0 = textfield
              | otherwise = textfield ! [ size (show c), maxlength c ]
