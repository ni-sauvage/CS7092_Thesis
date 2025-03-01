-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Obs.

module Obs.Abs where

import Prelude (Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Obs
    = ObsName ThreadId ObsIdent
    | ObsInit ThreadId
    | ObsTask ThreadId Taskname
    | ObsSignal ThreadId Integer
    | ObsDef ThreadId Varname Varval
    | ObsDecl ThreadId Typename Varname
    | ObsDeclVal ThreadId Typename Varname Varval
    | ObsDeclArr ThreadId Typename Varname SizeDcl
    | ObsCall ThreadId ObsIdent Args
    | ObsState ThreadId Integer StateObs
    | ObsStruct ThreadId Varname
    | ObsSeq ThreadId Varname Scalar
    | ObsPtr ThreadId Varname Varval
    | ObsScalar ThreadId Varname Varval
    | ObsScalarIndex ThreadId Varname Varindex Varval
    | ObsEnd ThreadId Varname
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Scalar
    = ObsScalarNone End | ObsScalarCons ThreadId Varval Scalar
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data End = ObsEndSeq ThreadId Varname
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Args
    = ArgsConsString ObsStr Args
    | ArgsConsInteger Integer Args
    | ArgsConsDouble Double Args
    | ArgsOne
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Taskname = TaskName ObsIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Varname = VarName ObsIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Varval
    = VarValInt Integer
    | VarValDouble Double
    | VarValNull
    | VarValStr ObsStr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Typename = TypeName ObsIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data StateObs = StateObs ObsIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Varindex = VarIndex Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ThreadId = ThreadId Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SizeDcl = SizeDclVar Varval | SizeDclDef ObsIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype ObsIdent = ObsIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype ObsStr = ObsStr String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

