-- | This abstract syntax tree for CSS selectors was created to help
-- me learn their syntax.
-- Model the syntax for the CSS 2.1 selectors, see http://www.w3.org/TR/CSS21/syndata.html
-- Mostly taken from http://www.rikkertkoppes.com/thoughts/css-syntax/ and the CSS 3 spec.
-- Using "show" will render them properly.
module CSS(myCSS) where

import Data.Char(isUpper)
import Control.Monad(guard)
import Text.ParserCombinators.Parsec(parse,digit,try,char,oneOf,noneOf,choice,string
                                    ,satisfy,eof,skipMany,skipMany1,optional,many1,(<|>))
import Data.Ix(inRange)
import Data.List(intersperse)
import Data.Monoid(Monoid(..))

myCSS = show $ Rules
  [r (c "center") [d "text-align" "center"
                  ]
  ,r (c "border") [d "border" (unwords ["solid","black"])
                  ,d "padding" "0.5em"
                  ]
  ,r (c "tt") [d "font-family" "monospace"
              ]
  ,r (c "left") [d "float" "left"
                ,d "width" "50%"
                ,d "margin" "0px"
                ,d "padding" "0px"
                ]
  ,r (c "right") [d "float" "right"
                 ,d "width" "50%"
                 ,d "margin" "0px"
                 ,d "padding" "0px"
                 ]
  ,r (i "results") [d "color" "black"
                   ,d "background-color" "white"
                   ]
  ,r (i "history") [d "color" "black"
                   ,d "background-color" "silver"
                   ]
  ]
 where r = Rule
       c a = selClass (ok1 a)
       i a = selID (ok1 a)
       d a b = Declare (ok2 a) (ok3 b)

       ok1 s = if all (`elem` (['a'..'z']++['0'..'9'])) s
                 then s else error ("Invalid css ok1 : "++s)
       ok2 s = if all (`elem` ("-"++['a'..'z']++['0'..'9'])) s
                 then s else error ("Invalid css ok1 : "++s)
       ok3 s = if all (`elem` ("- .%"++['a'..'z']++['0'..'9'])) s
                 then s else error ("Invalid css ok2 : "++s)

{-# INLINE concatShow #-}
concatShow :: (Show a) => [a] -> ShowS
concatShow = foldr (.) id . map (showsPrec 0)

{-# INLINE interShow #-}
interShow :: (Show a) => ShowS -> [a] -> ShowS
interShow del = foldr (.) id . intersperse del . map (showsPrec 0)

newtype Rules = Rules [Rule]

instance Monoid Rules where
  mempty = Rules mempty
  mappend (Rules a) (Rules b) = Rules (mappend a b)

instance Show Rules where
  showsPrec _ (Rules []) = id
  showsPrec p (Rules rs) = interShow ('\n':) rs . ('\n':)

data Rule = Rule Selectors [ Declaration ]

instance Show Rule where
  showsPrec p (Rule s ds) = showsPrec p s . (' ':) . showList ds

data Declaration = Declare PropertyString ValueString
                 | Important PropertyString ValueString

instance Show Declaration where
    showsPrec _ (Declare p v) = (p++) . (" : "++) . (v++)
    showsPrec _ (Important p v) = (p++) . (" : "++) . (v++) . (" !important"++)
    showList [] = ("{}"++)
    showList [x] = ("{ "++) . showsPrec 0 x . (" }"++)
    showList xs = ("{\n  "++) . interShow (";\n  "++) xs . ("\n}"++)

type PropertyString = String
type ValueString = String

newtype Selectors = Selectors [Selector]

instance Monoid Selectors where
  mempty = Selectors mempty
  mappend (Selectors a) (Selectors b) = Selectors (mappend a b)

instance Show Selectors where
  showsPrec _ (Selectors xs) = interShow (", "++) xs

data Selector = Selector (Maybe PseudoElement) SelItem

instance Show Selector where
  showsPrec p (Selector mpe item) = showsPrec p item . maybe id (showsPrec p) mpe

data PseudoElement = Pseudo_first'line
                   | Pseudo_first'letter
                   | Pseudo_before
                   | Pseudo_after

instance Show PseudoElement where
  showsPrec _ x = case x of
                     Pseudo_first'line   -> (":first_line"++)
                     Pseudo_first'letter -> (":first_letter"++)
                     Pseudo_before       -> (":before"++)
                     Pseudo_after        -> (":after"++)
  showList = concatShow

data SelItem = SelItem [(SelSeq,SelOp)] SelSeq -- emphasize last item

instance Show SelItem where
  showsPrec p (SelItem itemops selSeq) = 
    foldr (\(a,b) f -> showsPrec p a . showsPrec p b . f) (showsPrec p selSeq) itemops


data SelOp = SelOp_Descendant
           | SelOp_Child
           | SelOp_Adjacent
           | SelOp_Prior

instance Show SelOp where
  showsPrec p x = case x of
                    SelOp_Descendant -> (' ':)
                    SelOp_Child      -> (" > "++)
                    SelOp_Adjacent   -> (" + "++)
                    SelOp_Prior      -> (" ~ "++)


data SelSeq = SelSeq SelSeqType [SelMod]

instance Show SelSeq where
  showsPrec p (SelSeq SelAll []) = showsPrec p SelAll
  showsPrec _ (SelSeq SelAll sms) = showList sms
  showsPrec p (SelSeq selTag sms) = showsPrec p selTag . showList sms

data SelSeqType = SelAll
                | SelTag TagString

instance Show SelSeqType where
  showsPrec _ SelAll = ('*':)
  showsPrec p (SelTag ts) = showsPrec p ts

newtype TagString = TagString String

instance Show TagString where
  showsPrec _ (TagString s) = (s++)

data SelMod = SelAttr (Either String (String,SelAttrOp,String))
            | SelClass String
            | SelID String
            | SelPC PseudoClass

instance Show SelMod where
  showsPrec p x =
      case x of
             SelAttr (Left s) -> bk (s++)
             SelAttr (Right (s1,op,s2)) -> bk (showsPrec p s1 . showsPrec p op . showsPrec p s2)
             SelClass s -> ('.':) . (s++)
             SelID s -> ('#':) . (s++)
             SelPC pc -> showsPrec p pc
    where bk s =  ('[':) . s . (']':)
  showList sms = concatShow sms

data SelAttrOp = SelAttrEq
               | SelAttrMatches
               | SelAttrStarts

instance Show SelAttrOp where
  showsPrec _ x =
               case x of
                 SelAttrEq -> ('=':)
                 SelAttrMatches -> ('~':).('=':)
                 SelAttrStarts -> ('|':).('=':)

data PseudoClass = Pseudo_first'child
                 | Pseudo_link
                 | Pseudo_visited
                 | Pseudo_hover
                 | Pseudo_active
                 | Pseudo_focus
                 | Pseudo_lang String

instance Show PseudoClass where
  showsPrec _ x = case x of
    Pseudo_first'child  -> (":first_child"++)
    Pseudo_link         -> (":link"++)
    Pseudo_visited      -> (":visited"++)
    Pseudo_hover        -> (":hover"++)
    Pseudo_active       -> (":active"++)
    Pseudo_focus        -> (":focus"++)
    Pseudo_lang lang    -> ((":lang("++ lang ++ ")") ++)
  showList = concatShow

-- Write directly in abstract syntax tree form
s = Selectors [Selector (Just Pseudo_first'line)
                        (SelItem [(SelSeq (SelTag (TagString "div")) [SelID "history"
                                                                    ,SelClass "right"]
                                  ,SelOp_Child)
                                 ,(SelSeq SelAll []  -- all is '*'
                                  ,SelOp_Descendant)]
                                 (SelSeq SelAll [SelPC Pseudo_visited  -- do not print all as '*'
                                                ,SelPC Pseudo_hover]))
              ,Selector Nothing
                        (SelItem [] (SelSeq (SelTag (TagString "body")) []))]

test_s = show s == "div#history.right > * :visited:hover:first_line, body"

-- very simple smart constructors.  no string content checking yet
selTag s = Selectors [Selector Nothing (SelItem [] (SelSeq (SelTag (TagString s)) []))]
selClass s = Selectors [Selector Nothing (SelItem [] (SelSeq SelAll [SelClass s]))]
selID s = Selectors [Selector Nothing (SelItem [] (SelSeq SelAll [SelID s]))]
selTagClass t s = Selectors [Selector Nothing (SelItem [] (SelSeq (SelTag (TagString t)) [SelClass s]))]
selTagID t s = Selectors [Selector Nothing (SelItem [] (SelSeq (SelTag (TagString t)) [SelID s]))]

test_c = show (mconcat[ selTag "div", selID "history" `mappend` selClass "left"
                      , selTagClass "p" "tt", selTagID "div" "results" ])
       == "div, #history, .left, p.tt, div#results"


r = Rules [Rule (selTag "div") []
          ,Rule (selTagID "p" "tt") [Declare "font-family" "monospaced"]
          ,Rule (selClass "left") [Declare "float" "left"
                                  ,Important "width" "50%"
                                  ,Declare "margin" "0px"]]
test_r = show r == "div {}\n\
                   \p#tt { font-family : monospaced }\n\
                   \.left {\n\
                   \  float : left;\n\
                   \  width : 50% !important;\n\
                   \  margin : 0px\n\
                   \}"

is_valid_lower xs = all (not . isUpper) xs && is_valid_ident xs
is_valid_ident = either (const False) (const True) . parse is_ident "ident"

is_ident = optional (char '-') >> is_nmstart >> skipMany is_nmchar >> eof

is_name = skipMany1 is_nmchar

is_nmstart = choice [ skip $ oneOf letters, try is_nonascii, try is_escape]

is_nonascii = skip $ satisfy (> '\255')

is_unicode = do
  char '\\'
  vals <- many1 (oneOf hex)
  guard (inRange (1,6) (length vals))
  optional (try (skip $ string "\r\n") <|> skip (oneOf " \n\r\t\f"))

is_escape = choice [try is_unicode, try (char '\\' >> skip (noneOf ("\n\r\f"++hex)))]

is_nmchar = choice [skip $ oneOf ('-':(digits++letters)), is_nonascii, try is_escape]

skip p = p >> return ()

digits = ['0'..'9']
hex = digits++['a'..'f']++['A'..'F']
letters = '_':['a'..'z']++['A'..'Z']
