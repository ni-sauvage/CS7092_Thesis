-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Lexer definition for use with Alex 3
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module C.Lex where

import Prelude

import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
}

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \; | \, | \= | \{ | \} | \: | \( | \) | \[ | \] | \* | \. \. \. | \? | \| \| | \& \& | \| | \^ | \& | \= \= | \! \= | \< | \> | \< \= | \> \= | \< \< | \> \> | \+ | \- | \/ | \% | \+ \+ | \- \- | \. | \- \> | \~ | \! | \* \= | \/ \= | \% \= | \+ \= | \- \= | \< \< \= | \> \> \= | \& \= | \^ \= | \| \=

:-

-- Line comment "//"
"//" [.]* ;

-- Line comment "#"
"#" [.]* ;

-- Block comment "/*" "*/"
\/ \* [$u # \*]* \* ([$u # [\* \/]] [$u # \*]* \* | \*)* \/ ;

-- Whitespace (skipped)
$white+ ;

-- Symbols
@rsyms
    { tok (eitherResIdent TV) }

-- token Unsigned
[1 2 3 4 5 6 7 8 9]$d * [U u]
    { tok (eitherResIdent T_Unsigned) }

-- token Long
[1 2 3 4 5 6 7 8 9]$d * [L l]
    { tok (eitherResIdent T_Long) }

-- token UnsignedLong
[1 2 3 4 5 6 7 8 9]$d * (u l | U L)
    { tok (eitherResIdent T_UnsignedLong) }

-- token Hexadecimal
0 [X x]([A B C D E F a b c d e f]| $d)+
    { tok (eitherResIdent T_Hexadecimal) }

-- token HexUnsigned
0 [X x]([A B C D E F a b c d e f]| $d)+ [U u]
    { tok (eitherResIdent T_HexUnsigned) }

-- token HexLong
0 [X x]([A B C D E F a b c d e f]| $d)+ [L l]
    { tok (eitherResIdent T_HexLong) }

-- token HexUnsLong
0 [X x]([A B C D E F a b c d e f]| $d)+ (u l | U L)
    { tok (eitherResIdent T_HexUnsLong) }

-- token Octal
0 [0 1 2 3 4 5 6 7]*
    { tok (eitherResIdent T_Octal) }

-- token OctalUnsigned
0 [0 1 2 3 4 5 6 7]* [U u]
    { tok (eitherResIdent T_OctalUnsigned) }

-- token OctalLong
0 [0 1 2 3 4 5 6 7]* [L l]
    { tok (eitherResIdent T_OctalLong) }

-- token OctalUnsLong
0 [0 1 2 3 4 5 6 7]* (u l | U L)
    { tok (eitherResIdent T_OctalUnsLong) }

-- token CDouble
($d + \. | \. $d +)([E e]\- ? $d +)? | $d + [E e]\- ? $d + | $d + \. $d + E \- ? $d +
    { tok (eitherResIdent T_CDouble) }

-- token CFloat
($d + \. $d + | $d + \. | \. $d +)([E e]\- ? $d +)? [F f]| $d + [E e]\- ? $d + [F f]
    { tok (eitherResIdent T_CFloat) }

-- token CLongDouble
($d + \. $d + | $d + \. | \. $d +)([E e]\- ? $d +)? [L l]| $d + [E e]\- ? $d + [L l]
    { tok (eitherResIdent T_CLongDouble) }

-- token CIdent
(\_ | $l)([\' \_]| ($d | $l)) *
    { tok (eitherResIdent T_CIdent) }

-- Keywords and Ident
$l $i*
    { tok (eitherResIdent TV) }

-- String
\" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | r | f)))* \"
    { tok (TL . unescapeInitTail) }

-- Char
\' ($u # [\' \\] | \\ [\\ \' n t r f]) \'
    { tok TC }

-- Integer
$d+
    { tok TI }

-- Double
$d+ \. $d+ (e (\-)? $d+)?
    { tok TD }

{
-- | Create a token with position.
tok :: (String -> Tok) -> (Posn -> String -> Token)
tok f p = PT p . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !String                    -- ^ String literal.
  | TI !String                    -- ^ Integer literal.
  | TV !String                    -- ^ Identifier.
  | TD !String                    -- ^ Float literal.
  | TC !String                    -- ^ Character literal.
  | T_Unsigned !String
  | T_Long !String
  | T_UnsignedLong !String
  | T_Hexadecimal !String
  | T_HexUnsigned !String
  | T_HexLong !String
  | T_HexUnsLong !String
  | T_Octal !String
  | T_OctalUnsigned !String
  | T_OctalLong !String
  | T_OctalUnsLong !String
  | T_CDouble !String
  | T_CFloat !String
  | T_CLongDouble !String
  | T_CIdent !String
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: String -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: String
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p)  = p

-- | Get line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t = (tokenLineCol t, tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_Unsigned s) -> s
  PT _ (T_Long s) -> s
  PT _ (T_UnsignedLong s) -> s
  PT _ (T_Hexadecimal s) -> s
  PT _ (T_HexUnsigned s) -> s
  PT _ (T_HexLong s) -> s
  PT _ (T_HexUnsLong s) -> s
  PT _ (T_Octal s) -> s
  PT _ (T_OctalUnsigned s) -> s
  PT _ (T_OctalLong s) -> s
  PT _ (T_OctalUnsLong s) -> s
  PT _ (T_CDouble s) -> s
  PT _ (T_CFloat s) -> s
  PT _ (T_CLongDouble s) -> s
  PT _ (T_CIdent s) -> s

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = tokenText t

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B String Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b "auto" 44
    (b "/" 22
       (b "*=" 11
          (b "&&" 6
             (b "%" 3 (b "!=" 2 (b "!" 1 N N) N) (b "&" 5 (b "%=" 4 N N) N))
             (b ")" 9 (b "(" 8 (b "&=" 7 N N) N) (b "*" 10 N N)))
          (b "--" 17
             (b "+=" 14
                (b "++" 13 (b "+" 12 N N) N) (b "-" 16 (b "," 15 N N) N))
             (b "." 20 (b "->" 19 (b "-=" 18 N N) N) (b "..." 21 N N))))
       (b ">=" 33
          (b "<<=" 28
             (b ";" 25
                (b ":" 24 (b "/=" 23 N N) N) (b "<<" 27 (b "<" 26 N N) N))
             (b "==" 31 (b "=" 30 (b "<=" 29 N N) N) (b ">" 32 N N)))
          (b "Typedef_name" 39
             (b "?" 36
                (b ">>=" 35 (b ">>" 34 N N) N)
                (b "RtemsModelEventsMgr_Context" 38 (b "Context" 37 N N) N))
             (b "^" 42 (b "]" 41 (b "[" 40 N N) N) (b "^=" 43 N N)))))
    (b "rtems_status_code" 66
       (b "extern" 55
          (b "default" 50
             (b "char" 47
                (b "case" 46 (b "break" 45 N N) N)
                (b "continue" 49 (b "const" 48 N N) N))
             (b "else" 53 (b "double" 52 (b "do" 51 N N) N) (b "enum" 54 N N)))
          (b "long" 61
             (b "goto" 58
                (b "for" 57 (b "float" 56 N N) N) (b "int" 60 (b "if" 59 N N) N))
             (b "rtems_event_set" 64
                (b "return" 63 (b "register" 62 N N) N) (b "rtems_id" 65 N N))))
       (b "union" 77
          (b "sizeof" 72
             (b "short" 69
                (b "rtems_task_priority" 68 (b "rtems_task_argument" 67 N N) N)
                (b "size_t" 71 (b "signed" 70 N N) N))
             (b "switch" 75
                (b "struct" 74 (b "static" 73 N N) N) (b "typedef" 76 N N)))
          (b "|" 83
             (b "volatile" 80
                (b "void" 79 (b "unsigned" 78 N N) N)
                (b "{" 82 (b "while" 81 N N) N))
             (b "}" 86 (b "||" 85 (b "|=" 84 N N) N) (b "~" 87 N N)))))
  where
  b s n = B bs (TS bs n)
    where
    bs = s

-- | Unquote string literal.
unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
