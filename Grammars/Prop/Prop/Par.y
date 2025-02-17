-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Prop.Par
  ( happyError
  , myLexer
  , pListExp
  , pExp
  , pExp2
  , pExp3
  , pExp4
  , pExp5
  , pExp6
  , pExp7
  , pExp8
  , pExp9
  , pExp10
  , pExp11
  , pExp12
  , pExp13
  , pExp14
  , pExp15
  , pQuant
  , pListCIdent
  , pConstant
  , pExp1
  , pUnary_operator
  ) where

import Prelude

import qualified Prop.Abs
import Prop.Lex

}

%name pListExp ListExp
%name pExp Exp
%name pExp2 Exp2
%name pExp3 Exp3
%name pExp4 Exp4
%name pExp5 Exp5
%name pExp6 Exp6
%name pExp7 Exp7
%name pExp8 Exp8
%name pExp9 Exp9
%name pExp10 Exp10
%name pExp11 Exp11
%name pExp12 Exp12
%name pExp13 Exp13
%name pExp14 Exp14
%name pExp15 Exp15
%name pQuant Quant
%name pListCIdent ListCIdent
%name pConstant Constant
%name pExp1 Exp1
%name pUnary_operator Unary_operator
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'             { PT _ (TS _ 1)             }
  '!='            { PT _ (TS _ 2)             }
  '%'             { PT _ (TS _ 3)             }
  '&'             { PT _ (TS _ 4)             }
  '&&'            { PT _ (TS _ 5)             }
  '('             { PT _ (TS _ 6)             }
  ')'             { PT _ (TS _ 7)             }
  '*'             { PT _ (TS _ 8)             }
  '+'             { PT _ (TS _ 9)             }
  '++'            { PT _ (TS _ 10)            }
  ','             { PT _ (TS _ 11)            }
  '-'             { PT _ (TS _ 12)            }
  '--'            { PT _ (TS _ 13)            }
  '->'            { PT _ (TS _ 14)            }
  '.'             { PT _ (TS _ 15)            }
  '/'             { PT _ (TS _ 16)            }
  '<'             { PT _ (TS _ 17)            }
  '<<'            { PT _ (TS _ 18)            }
  '<='            { PT _ (TS _ 19)            }
  '=='            { PT _ (TS _ 20)            }
  '==>'           { PT _ (TS _ 21)            }
  '>'             { PT _ (TS _ 22)            }
  '>='            { PT _ (TS _ 23)            }
  '>>'            { PT _ (TS _ 24)            }
  '['             { PT _ (TS _ 25)            }
  ']'             { PT _ (TS _ 26)            }
  '^'             { PT _ (TS _ 27)            }
  'exists'        { PT _ (TS _ 28)            }
  'forall'        { PT _ (TS _ 29)            }
  '|'             { PT _ (TS _ 30)            }
  '||'            { PT _ (TS _ 31)            }
  '~'             { PT _ (TS _ 32)            }
  L_charac        { PT _ (TC $$)              }
  L_doubl         { PT _ (TD $$)              }
  L_integ         { PT _ (TI $$)              }
  L_quoted        { PT _ (TL $$)              }
  L_Unsigned      { PT _ (T_Unsigned $$)      }
  L_Long          { PT _ (T_Long $$)          }
  L_UnsignedLong  { PT _ (T_UnsignedLong $$)  }
  L_Hexadecimal   { PT _ (T_Hexadecimal $$)   }
  L_HexUnsigned   { PT _ (T_HexUnsigned $$)   }
  L_HexLong       { PT _ (T_HexLong $$)       }
  L_HexUnsLong    { PT _ (T_HexUnsLong $$)    }
  L_Octal         { PT _ (T_Octal $$)         }
  L_OctalUnsigned { PT _ (T_OctalUnsigned $$) }
  L_OctalLong     { PT _ (T_OctalLong $$)     }
  L_OctalUnsLong  { PT _ (T_OctalUnsLong $$)  }
  L_CDouble       { PT _ (T_CDouble $$)       }
  L_CFloat        { PT _ (T_CFloat $$)        }
  L_CLongDouble   { PT _ (T_CLongDouble $$)   }
  L_CIdent        { PT _ (T_CIdent $$)        }

%%

Char    :: { Char }
Char     : L_charac { (read $1) :: Char }

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

Unsigned :: { Prop.Abs.Unsigned }
Unsigned  : L_Unsigned { Prop.Abs.Unsigned $1 }

Long :: { Prop.Abs.Long }
Long  : L_Long { Prop.Abs.Long $1 }

UnsignedLong :: { Prop.Abs.UnsignedLong }
UnsignedLong  : L_UnsignedLong { Prop.Abs.UnsignedLong $1 }

Hexadecimal :: { Prop.Abs.Hexadecimal }
Hexadecimal  : L_Hexadecimal { Prop.Abs.Hexadecimal $1 }

HexUnsigned :: { Prop.Abs.HexUnsigned }
HexUnsigned  : L_HexUnsigned { Prop.Abs.HexUnsigned $1 }

HexLong :: { Prop.Abs.HexLong }
HexLong  : L_HexLong { Prop.Abs.HexLong $1 }

HexUnsLong :: { Prop.Abs.HexUnsLong }
HexUnsLong  : L_HexUnsLong { Prop.Abs.HexUnsLong $1 }

Octal :: { Prop.Abs.Octal }
Octal  : L_Octal { Prop.Abs.Octal $1 }

OctalUnsigned :: { Prop.Abs.OctalUnsigned }
OctalUnsigned  : L_OctalUnsigned { Prop.Abs.OctalUnsigned $1 }

OctalLong :: { Prop.Abs.OctalLong }
OctalLong  : L_OctalLong { Prop.Abs.OctalLong $1 }

OctalUnsLong :: { Prop.Abs.OctalUnsLong }
OctalUnsLong  : L_OctalUnsLong { Prop.Abs.OctalUnsLong $1 }

CDouble :: { Prop.Abs.CDouble }
CDouble  : L_CDouble { Prop.Abs.CDouble $1 }

CFloat :: { Prop.Abs.CFloat }
CFloat  : L_CFloat { Prop.Abs.CFloat $1 }

CLongDouble :: { Prop.Abs.CLongDouble }
CLongDouble  : L_CLongDouble { Prop.Abs.CLongDouble $1 }

CIdent :: { Prop.Abs.CIdent }
CIdent  : L_CIdent { Prop.Abs.CIdent $1 }

ListExp :: { [Prop.Abs.Exp] }
ListExp
  : Exp ListExp { (:) $1 $2 }
  | {- empty -} { [] }
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }

Exp :: { Prop.Abs.Exp }
Exp : Exp1 '==>' Exp2 { Prop.Abs.Eimpl $1 $3 } | Exp1 { $1 }

Exp2 :: { Prop.Abs.Exp }
Exp2 : Exp2 '||' Exp3 { Prop.Abs.Elor $1 $3 } | Exp3 { $1 }

Exp3 :: { Prop.Abs.Exp }
Exp3 : Exp3 '&&' Exp4 { Prop.Abs.Eland $1 $3 } | Exp4 { $1 }

Exp4 :: { Prop.Abs.Exp }
Exp4 : Exp4 '|' Exp5 { Prop.Abs.Ebitor $1 $3 } | Exp5 { $1 }

Exp5 :: { Prop.Abs.Exp }
Exp5 : Exp5 '^' Exp6 { Prop.Abs.Ebitexor $1 $3 } | Exp6 { $1 }

Exp6 :: { Prop.Abs.Exp }
Exp6 : Exp6 '&' Exp7 { Prop.Abs.Ebitand $1 $3 } | Exp7 { $1 }

Exp7 :: { Prop.Abs.Exp }
Exp7
  : Exp7 '==' Exp8 { Prop.Abs.Eeq $1 $3 }
  | Exp7 '!=' Exp8 { Prop.Abs.Eneq $1 $3 }
  | Exp8 { $1 }

Exp8 :: { Prop.Abs.Exp }
Exp8
  : Exp8 '<' Exp9 { Prop.Abs.Elthen $1 $3 }
  | Exp8 '>' Exp9 { Prop.Abs.Egrthen $1 $3 }
  | Exp9 { $1 }

Exp9 :: { Prop.Abs.Exp }
Exp9
  : Exp9 '<=' Exp10 { Prop.Abs.Ele $1 $3 }
  | Exp9 '>=' Exp10 { Prop.Abs.Ege $1 $3 }
  | Exp10 { $1 }

Exp10 :: { Prop.Abs.Exp }
Exp10
  : Exp10 '<<' Exp11 { Prop.Abs.Eleft $1 $3 }
  | Exp10 '>>' Exp11 { Prop.Abs.Eright $1 $3 }
  | Exp11 { $1 }

Exp11 :: { Prop.Abs.Exp }
Exp11
  : Exp11 '+' Exp12 { Prop.Abs.Eplus $1 $3 }
  | Exp11 '-' Exp12 { Prop.Abs.Eminus $1 $3 }
  | Exp12 { $1 }

Exp12 :: { Prop.Abs.Exp }
Exp12
  : Exp12 '*' Exp13 { Prop.Abs.Etimes $1 $3 }
  | Exp12 '/' Exp13 { Prop.Abs.Ediv $1 $3 }
  | Exp12 '%' Exp13 { Prop.Abs.Emod $1 $3 }
  | Exp13 { $1 }

Exp13 :: { Prop.Abs.Exp }
Exp13
  : '++' Exp14 { Prop.Abs.Epreinc $2 }
  | '--' Exp14 { Prop.Abs.Epredec $2 }
  | Unary_operator Exp13 { Prop.Abs.Epreop $1 $2 }
  | Exp14 { $1 }

Exp14 :: { Prop.Abs.Exp }
Exp14
  : Exp14 '[' Exp ']' { Prop.Abs.Earray $1 $3 }
  | Exp14 '(' ')' { Prop.Abs.Efunk $1 }
  | Exp14 '(' ListExp ')' { Prop.Abs.Efunkpar $1 $3 }
  | Exp14 '.' CIdent { Prop.Abs.Eselect $1 $3 }
  | Exp14 '->' CIdent { Prop.Abs.Epoint $1 $3 }
  | Exp14 '++' { Prop.Abs.Epostinc $1 }
  | Exp14 '--' { Prop.Abs.Epostdec $1 }
  | Exp15 { $1 }

Exp15 :: { Prop.Abs.Exp }
Exp15
  : Quant ListCIdent '|' Exp { Prop.Abs.Equant $1 $2 $4 }
  | CIdent { Prop.Abs.Evar $1 }
  | Constant { Prop.Abs.Econst $1 }
  | String { Prop.Abs.Estring $1 }
  | '(' Exp ')' { $2 }

Quant :: { Prop.Abs.Quant }
Quant : 'exists' { Prop.Abs.Qexists } | 'forall' { Prop.Abs.Qall }

ListCIdent :: { [Prop.Abs.CIdent] }
ListCIdent
  : CIdent { (:[]) $1 } | CIdent ',' ListCIdent { (:) $1 $3 }

Constant :: { Prop.Abs.Constant }
Constant
  : Double { Prop.Abs.Efloat $1 }
  | Char { Prop.Abs.Echar $1 }
  | Unsigned { Prop.Abs.Eunsigned $1 }
  | Long { Prop.Abs.Elong $1 }
  | UnsignedLong { Prop.Abs.Eunsignlong $1 }
  | Hexadecimal { Prop.Abs.Ehexadec $1 }
  | HexUnsigned { Prop.Abs.Ehexaunsign $1 }
  | HexLong { Prop.Abs.Ehexalong $1 }
  | HexUnsLong { Prop.Abs.Ehexaunslong $1 }
  | Octal { Prop.Abs.Eoctal $1 }
  | OctalUnsigned { Prop.Abs.Eoctalunsign $1 }
  | OctalLong { Prop.Abs.Eoctallong $1 }
  | OctalUnsLong { Prop.Abs.Eoctalunslong $1 }
  | CDouble { Prop.Abs.Ecdouble $1 }
  | CFloat { Prop.Abs.Ecfloat $1 }
  | CLongDouble { Prop.Abs.Eclongdouble $1 }
  | Integer { Prop.Abs.Eint $1 }

Exp1 :: { Prop.Abs.Exp }
Exp1 : Exp2 { $1 }

Unary_operator :: { Prop.Abs.Unary_operator }
Unary_operator
  : '&' { Prop.Abs.Address }
  | '*' { Prop.Abs.Indirection }
  | '+' { Prop.Abs.Plus }
  | '-' { Prop.Abs.Negative }
  | '~' { Prop.Abs.Complement }
  | '!' { Prop.Abs.Logicalneg }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

