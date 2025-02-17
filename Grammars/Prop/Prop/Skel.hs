-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Prop.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Prop.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transUnsigned :: Prop.Abs.Unsigned -> Result
transUnsigned x = case x of
  Prop.Abs.Unsigned string -> failure x

transLong :: Prop.Abs.Long -> Result
transLong x = case x of
  Prop.Abs.Long string -> failure x

transUnsignedLong :: Prop.Abs.UnsignedLong -> Result
transUnsignedLong x = case x of
  Prop.Abs.UnsignedLong string -> failure x

transHexadecimal :: Prop.Abs.Hexadecimal -> Result
transHexadecimal x = case x of
  Prop.Abs.Hexadecimal string -> failure x

transHexUnsigned :: Prop.Abs.HexUnsigned -> Result
transHexUnsigned x = case x of
  Prop.Abs.HexUnsigned string -> failure x

transHexLong :: Prop.Abs.HexLong -> Result
transHexLong x = case x of
  Prop.Abs.HexLong string -> failure x

transHexUnsLong :: Prop.Abs.HexUnsLong -> Result
transHexUnsLong x = case x of
  Prop.Abs.HexUnsLong string -> failure x

transOctal :: Prop.Abs.Octal -> Result
transOctal x = case x of
  Prop.Abs.Octal string -> failure x

transOctalUnsigned :: Prop.Abs.OctalUnsigned -> Result
transOctalUnsigned x = case x of
  Prop.Abs.OctalUnsigned string -> failure x

transOctalLong :: Prop.Abs.OctalLong -> Result
transOctalLong x = case x of
  Prop.Abs.OctalLong string -> failure x

transOctalUnsLong :: Prop.Abs.OctalUnsLong -> Result
transOctalUnsLong x = case x of
  Prop.Abs.OctalUnsLong string -> failure x

transCDouble :: Prop.Abs.CDouble -> Result
transCDouble x = case x of
  Prop.Abs.CDouble string -> failure x

transCFloat :: Prop.Abs.CFloat -> Result
transCFloat x = case x of
  Prop.Abs.CFloat string -> failure x

transCLongDouble :: Prop.Abs.CLongDouble -> Result
transCLongDouble x = case x of
  Prop.Abs.CLongDouble string -> failure x

transCIdent :: Prop.Abs.CIdent -> Result
transCIdent x = case x of
  Prop.Abs.CIdent string -> failure x

transExp :: Prop.Abs.Exp -> Result
transExp x = case x of
  Prop.Abs.Eimpl exp1 exp2 -> failure x
  Prop.Abs.Elor exp1 exp2 -> failure x
  Prop.Abs.Eland exp1 exp2 -> failure x
  Prop.Abs.Ebitor exp1 exp2 -> failure x
  Prop.Abs.Ebitexor exp1 exp2 -> failure x
  Prop.Abs.Ebitand exp1 exp2 -> failure x
  Prop.Abs.Eeq exp1 exp2 -> failure x
  Prop.Abs.Eneq exp1 exp2 -> failure x
  Prop.Abs.Elthen exp1 exp2 -> failure x
  Prop.Abs.Egrthen exp1 exp2 -> failure x
  Prop.Abs.Ele exp1 exp2 -> failure x
  Prop.Abs.Ege exp1 exp2 -> failure x
  Prop.Abs.Eleft exp1 exp2 -> failure x
  Prop.Abs.Eright exp1 exp2 -> failure x
  Prop.Abs.Eplus exp1 exp2 -> failure x
  Prop.Abs.Eminus exp1 exp2 -> failure x
  Prop.Abs.Etimes exp1 exp2 -> failure x
  Prop.Abs.Ediv exp1 exp2 -> failure x
  Prop.Abs.Emod exp1 exp2 -> failure x
  Prop.Abs.Epreinc exp -> failure x
  Prop.Abs.Epredec exp -> failure x
  Prop.Abs.Epreop unaryoperator exp -> failure x
  Prop.Abs.Earray exp1 exp2 -> failure x
  Prop.Abs.Efunk exp -> failure x
  Prop.Abs.Efunkpar exp exps -> failure x
  Prop.Abs.Eselect exp cident -> failure x
  Prop.Abs.Epoint exp cident -> failure x
  Prop.Abs.Epostinc exp -> failure x
  Prop.Abs.Epostdec exp -> failure x
  Prop.Abs.Equant quant cidents exp -> failure x
  Prop.Abs.Evar cident -> failure x
  Prop.Abs.Econst constant -> failure x
  Prop.Abs.Estring string -> failure x

transQuant :: Prop.Abs.Quant -> Result
transQuant x = case x of
  Prop.Abs.Qexists -> failure x
  Prop.Abs.Qall -> failure x

transConstant :: Prop.Abs.Constant -> Result
transConstant x = case x of
  Prop.Abs.Efloat double -> failure x
  Prop.Abs.Echar char -> failure x
  Prop.Abs.Eunsigned unsigned -> failure x
  Prop.Abs.Elong long -> failure x
  Prop.Abs.Eunsignlong unsignedlong -> failure x
  Prop.Abs.Ehexadec hexadecimal -> failure x
  Prop.Abs.Ehexaunsign hexunsigned -> failure x
  Prop.Abs.Ehexalong hexlong -> failure x
  Prop.Abs.Ehexaunslong hexunslong -> failure x
  Prop.Abs.Eoctal octal -> failure x
  Prop.Abs.Eoctalunsign octalunsigned -> failure x
  Prop.Abs.Eoctallong octallong -> failure x
  Prop.Abs.Eoctalunslong octalunslong -> failure x
  Prop.Abs.Ecdouble cdouble -> failure x
  Prop.Abs.Ecfloat cfloat -> failure x
  Prop.Abs.Eclongdouble clongdouble -> failure x
  Prop.Abs.Eint integer -> failure x

transUnary_operator :: Prop.Abs.Unary_operator -> Result
transUnary_operator x = case x of
  Prop.Abs.Address -> failure x
  Prop.Abs.Indirection -> failure x
  Prop.Abs.Plus -> failure x
  Prop.Abs.Negative -> failure x
  Prop.Abs.Complement -> failure x
  Prop.Abs.Logicalneg -> failure x
