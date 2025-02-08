-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Pml.

module Pml.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Pml.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Pml.Abs.PIdent where
  prt _ (Pml.Abs.PIdent i) = doc $ showString i
instance Print [Pml.Abs.Module] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Pml.Abs.Visible where
  prt i = \case
    Pml.Abs.Visible_hidden -> prPrec i 0 (concatD [doc (showString "hidden")])
    Pml.Abs.Visible_show -> prPrec i 0 (concatD [doc (showString "show")])

instance Print Pml.Abs.ChanPoll where
  prt i = \case
    Pml.Abs.ChanPoll_full -> prPrec i 0 (concatD [doc (showString "full")])
    Pml.Abs.ChanPoll1 -> prPrec i 0 (concatD [doc (showString "empty"), doc (showString "nfull"), doc (showString "nempty")])

instance Print Pml.Abs.Separator where
  prt i = \case
    Pml.Abs.Separator1 -> prPrec i 0 (concatD [doc (showString ";")])
    Pml.Abs.Separator2 -> prPrec i 0 (concatD [doc (showString "->")])

instance Print Pml.Abs.Typename where
  prt i = \case
    Pml.Abs.Typename_bit -> prPrec i 0 (concatD [doc (showString "bit")])
    Pml.Abs.Typename_bool -> prPrec i 0 (concatD [doc (showString "bool")])
    Pml.Abs.Typename_byte -> prPrec i 0 (concatD [doc (showString "byte")])
    Pml.Abs.Typename_short -> prPrec i 0 (concatD [doc (showString "short")])
    Pml.Abs.Typename_int -> prPrec i 0 (concatD [doc (showString "int")])
    Pml.Abs.Typename_mtype -> prPrec i 0 (concatD [doc (showString "mtype")])
    Pml.Abs.Typename_chan -> prPrec i 0 (concatD [doc (showString "chan")])
    Pml.Abs.TypenameUname uname -> prPrec i 0 (concatD [prt 0 uname])

instance Print Pml.Abs.AndOr where
  prt i = \case
    Pml.Abs.AndOr1 -> prPrec i 0 (concatD [doc (showString "&&")])
    Pml.Abs.AndOr2 -> prPrec i 0 (concatD [doc (showString "||")])

instance Print Pml.Abs.BinOp where
  prt i = \case
    Pml.Abs.BinOp1 -> prPrec i 0 (concatD [doc (showString "+")])
    Pml.Abs.BinOp2 -> prPrec i 0 (concatD [doc (showString "-")])
    Pml.Abs.BinOp3 -> prPrec i 0 (concatD [doc (showString "*")])
    Pml.Abs.BinOp4 -> prPrec i 0 (concatD [doc (showString "/")])
    Pml.Abs.BinOp5 -> prPrec i 0 (concatD [doc (showString "%")])
    Pml.Abs.BinOp6 -> prPrec i 0 (concatD [doc (showString "&")])
    Pml.Abs.BinOp7 -> prPrec i 0 (concatD [doc (showString "^")])
    Pml.Abs.BinOp8 -> prPrec i 0 (concatD [doc (showString "|")])
    Pml.Abs.BinOp9 -> prPrec i 0 (concatD [doc (showString ">")])
    Pml.Abs.BinOp10 -> prPrec i 0 (concatD [doc (showString "<")])
    Pml.Abs.BinOp11 -> prPrec i 0 (concatD [doc (showString ">=")])
    Pml.Abs.BinOp12 -> prPrec i 0 (concatD [doc (showString "<=")])
    Pml.Abs.BinOp13 -> prPrec i 0 (concatD [doc (showString "==")])
    Pml.Abs.BinOp14 -> prPrec i 0 (concatD [doc (showString "!=")])
    Pml.Abs.BinOp15 -> prPrec i 0 (concatD [doc (showString "<<")])
    Pml.Abs.BinOp16 -> prPrec i 0 (concatD [doc (showString ">>")])
    Pml.Abs.BinOpAndOr andor -> prPrec i 0 (concatD [prt 0 andor])

instance Print Pml.Abs.UnrOp where
  prt i = \case
    Pml.Abs.UnrOp1 -> prPrec i 0 (concatD [doc (showString "~")])
    Pml.Abs.UnrOp2 -> prPrec i 0 (concatD [doc (showString "-")])
    Pml.Abs.UnrOp3 -> prPrec i 0 (concatD [doc (showString "!")])

instance Print Pml.Abs.Const where
  prt i = \case
    Pml.Abs.Const_true -> prPrec i 0 (concatD [doc (showString "true")])
    Pml.Abs.Const_false -> prPrec i 0 (concatD [doc (showString "false")])
    Pml.Abs.Const_skip -> prPrec i 0 (concatD [doc (showString "skip")])
    Pml.Abs.ConstInteger n -> prPrec i 0 (concatD [prt 0 n])

instance Print Pml.Abs.PrintType where
  prt i = \case
    Pml.Abs.PrintType_print -> prPrec i 0 (concatD [doc (showString "print")])
    Pml.Abs.PrintType_printf -> prPrec i 0 (concatD [doc (showString "printf")])
    Pml.Abs.PrintType_printm -> prPrec i 0 (concatD [doc (showString "printm")])

instance Print Pml.Abs.Module where
  prt i = \case
    Pml.Abs.Mproc proctype -> prPrec i 0 (concatD [prt 0 proctype])
    Pml.Abs.Minline inline -> prPrec i 0 (concatD [prt 0 inline])
    Pml.Abs.Minit init -> prPrec i 0 (concatD [prt 0 init])
    Pml.Abs.Mnever never -> prPrec i 0 (concatD [prt 0 never])
    Pml.Abs.Mtrace trace -> prPrec i 0 (concatD [prt 0 trace])
    Pml.Abs.Mutype utype -> prPrec i 0 (concatD [prt 0 utype])
    Pml.Abs.Mmtype mtype -> prPrec i 0 (concatD [prt 0 mtype])
    Pml.Abs.MdeclList decllist -> prPrec i 0 (concatD [prt 0 decllist])

instance Print Pml.Abs.Proctype where
  prt i = \case
    Pml.Abs.Ptype pactive name pdecllist ppriority penabler sequence -> prPrec i 0 (concatD [prt 0 pactive, doc (showString "proctype"), prt 0 name, doc (showString "("), prt 0 pdecllist, doc (showString ")"), prt 0 ppriority, prt 0 penabler, doc (showString "{"), prt 0 sequence, doc (showString "}")])

instance Print Pml.Abs.Inline where
  prt i = \case
    Pml.Abs.Iline name arglist sequence -> prPrec i 0 (concatD [doc (showString "inline"), prt 0 name, doc (showString "("), prt 0 arglist, doc (showString ")"), doc (showString "{"), prt 0 sequence, doc (showString "}")])

instance Print Pml.Abs.Pactive where
  prt i = \case
    Pml.Abs.PactiveNone -> prPrec i 0 (concatD [])
    Pml.Abs.PactiveOne active -> prPrec i 0 (concatD [prt 0 active])

instance Print Pml.Abs.PdeclList where
  prt i = \case
    Pml.Abs.PdeclListNone -> prPrec i 0 (concatD [])
    Pml.Abs.PdeclListOne decllist -> prPrec i 0 (concatD [prt 0 decllist])

instance Print Pml.Abs.Ppriority where
  prt i = \case
    Pml.Abs.PpriorityNone -> prPrec i 0 (concatD [])
    Pml.Abs.PpriorityOne priority -> prPrec i 0 (concatD [prt 0 priority])

instance Print Pml.Abs.Penabler where
  prt i = \case
    Pml.Abs.PenablerNone -> prPrec i 0 (concatD [])
    Pml.Abs.PenablerOne enabler -> prPrec i 0 (concatD [prt 0 enabler])

instance Print Pml.Abs.Init where
  prt i = \case
    Pml.Abs.Initialise ipriority sequence -> prPrec i 0 (concatD [doc (showString "init"), prt 0 ipriority, doc (showString "{"), prt 0 sequence, doc (showString "}")])

instance Print Pml.Abs.Ipriority where
  prt i = \case
    Pml.Abs.IpriorityNone -> prPrec i 0 (concatD [])
    Pml.Abs.IpriorityOne priority -> prPrec i 0 (concatD [prt 0 priority])

instance Print Pml.Abs.Never where
  prt i = \case
    Pml.Abs.Nvr sequence -> prPrec i 0 (concatD [doc (showString "never"), doc (showString "{"), prt 0 sequence, doc (showString "}")])

instance Print Pml.Abs.Trace where
  prt i = \case
    Pml.Abs.Trc sequence -> prPrec i 0 (concatD [doc (showString "trace"), doc (showString "{"), prt 0 sequence, doc (showString "}")])

instance Print Pml.Abs.Utype where
  prt i = \case
    Pml.Abs.Utp name decllist -> prPrec i 0 (concatD [doc (showString "typedef"), prt 0 name, doc (showString "{"), prt 0 decllist, doc (showString "}"), doc (showString ";")])

instance Print Pml.Abs.Mtype where
  prt i = \case
    Pml.Abs.Mtp mequals mname msep -> prPrec i 0 (concatD [doc (showString "mtype"), prt 0 mequals, doc (showString "{"), prt 0 mname, doc (showString "}"), prt 0 msep])

instance Print Pml.Abs.Msep where
  prt i = \case
    Pml.Abs.MsepNone -> prPrec i 0 (concatD [])
    Pml.Abs.MsepOne -> prPrec i 0 (concatD [doc (showString ";")])

instance Print Pml.Abs.Mequals where
  prt i = \case
    Pml.Abs.Meq -> prPrec i 0 (concatD [])

instance Print Pml.Abs.Mname where
  prt i = \case
    Pml.Abs.MnameOne name -> prPrec i 0 (concatD [prt 0 name])
    Pml.Abs.Mnamecons name mname -> prPrec i 0 (concatD [prt 0 name, doc (showString ","), prt 0 mname])

instance Print Pml.Abs.DeclList where
  prt i = \case
    Pml.Abs.DclListOne decl separator -> prPrec i 0 (concatD [prt 0 decl, prt 0 separator])
    Pml.Abs.DclListOneNoSep decl -> prPrec i 0 (concatD [prt 0 decl])
    Pml.Abs.DclListCons decl separator decllist -> prPrec i 0 (concatD [prt 0 decl, prt 0 separator, prt 0 decllist])

instance Print Pml.Abs.Decl where
  prt i = \case
    Pml.Abs.DclOne declvisible typename dclivar -> prPrec i 0 (concatD [prt 0 declvisible, prt 0 typename, prt 0 dclivar])
    Pml.Abs.DclOneUnsigned declvisible unsigneddecl -> prPrec i 0 (concatD [prt 0 declvisible, prt 0 unsigneddecl])

instance Print Pml.Abs.DclIvar where
  prt i = \case
    Pml.Abs.DclIvarCons ivar dclivar -> prPrec i 0 (concatD [prt 0 ivar, doc (showString ","), prt 0 dclivar])
    Pml.Abs.DclIvarSub ivar -> prPrec i 0 (concatD [prt 0 ivar])

instance Print Pml.Abs.DeclVisible where
  prt i = \case
    Pml.Abs.DclVisNone -> prPrec i 0 (concatD [])
    Pml.Abs.DclVisOne visible -> prPrec i 0 (concatD [prt 0 visible])

instance Print Pml.Abs.UnsignedDecl where
  prt i = \case
    Pml.Abs.UDcl name const udclassign -> prPrec i 0 (concatD [doc (showString "unsigned"), prt 0 name, doc (showString ":"), prt 0 const, prt 0 udclassign])

instance Print Pml.Abs.UDclAssign where
  prt i = \case
    Pml.Abs.UDclAssignNone -> prPrec i 0 (concatD [])
    Pml.Abs.UdclAssignOne anyexpr -> prPrec i 0 (concatD [doc (showString "="), prt 0 anyexpr])

instance Print Pml.Abs.Active where
  prt i = \case
    Pml.Abs.Active aconst -> prPrec i 0 (concatD [doc (showString "active"), prt 0 aconst])

instance Print Pml.Abs.AConst where
  prt i = \case
    Pml.Abs.AconstNone -> prPrec i 0 (concatD [])
    Pml.Abs.AconstOne const -> prPrec i 0 (concatD [doc (showString "["), prt 0 const, doc (showString "]")])

instance Print Pml.Abs.Priority where
  prt i = \case
    Pml.Abs.Priority const -> prPrec i 0 (concatD [doc (showString "priority"), prt 0 const])

instance Print Pml.Abs.Enabler where
  prt i = \case
    Pml.Abs.Enabler expr -> prPrec i 0 (concatD [doc (showString "provided"), doc (showString "("), prt 0 expr, doc (showString ")")])

instance Print Pml.Abs.Sequence where
  prt i = \case
    Pml.Abs.SeqOne step -> prPrec i 0 (concatD [prt 0 step])
    Pml.Abs.SeqOneSep step separator -> prPrec i 0 (concatD [prt 0 step, prt 0 separator])
    Pml.Abs.SeqNoStep step sequence -> prPrec i 0 (concatD [prt 0 step, prt 0 sequence])
    Pml.Abs.SeqCons step separator sequence -> prPrec i 0 (concatD [prt 0 step, prt 0 separator, prt 0 sequence])

instance Print Pml.Abs.UStmt where
  prt i = \case
    Pml.Abs.UStmtNone -> prPrec i 0 (concatD [])
    Pml.Abs.UStmtOne stmt -> prPrec i 0 (concatD [doc (showString "unless"), prt 0 stmt])

instance Print Pml.Abs.Step where
  prt i = \case
    Pml.Abs.StepMType mtype -> prPrec i 0 (concatD [prt 0 mtype])
    Pml.Abs.StepStmt stmt ustmt -> prPrec i 0 (concatD [prt 0 stmt, prt 0 ustmt])
    Pml.Abs.StepDclList decllist -> prPrec i 0 (concatD [prt 0 decllist])
    Pml.Abs.StepXR varreflist -> prPrec i 0 (concatD [doc (showString "xr"), prt 0 varreflist])
    Pml.Abs.StepXS varreflist -> prPrec i 0 (concatD [doc (showString "xs"), prt 0 varreflist])

instance Print Pml.Abs.VarRefList where
  prt i = \case
    Pml.Abs.VarRefListOne varref -> prPrec i 0 (concatD [prt 0 varref])
    Pml.Abs.VarRefListCons varref varreflist -> prPrec i 0 (concatD [prt 0 varref, doc (showString ","), prt 0 varreflist])

instance Print Pml.Abs.AnyExpr where
  prt i = \case
    Pml.Abs.AnyExprParen anyexpr -> prPrec i 0 (concatD [doc (showString "("), prt 0 anyexpr, doc (showString ")")])
    Pml.Abs.AnyExprBinOp anyexpr1 binop anyexpr2 -> prPrec i 0 (concatD [prt 0 anyexpr1, prt 0 binop, prt 0 anyexpr2])
    Pml.Abs.AnyExprUnrOp unrop anyexpr -> prPrec i 0 (concatD [prt 0 unrop, prt 0 anyexpr])
    Pml.Abs.AnyExprCond anyexpr1 anyexpr2 anyexpr3 -> prPrec i 0 (concatD [doc (showString "("), prt 0 anyexpr1, doc (showString "->"), prt 0 anyexpr2, doc (showString ":"), prt 0 anyexpr3, doc (showString ")")])
    Pml.Abs.AnyExprLen varref -> prPrec i 0 (concatD [doc (showString "len"), doc (showString "("), prt 0 varref, doc (showString ")")])
    Pml.Abs.AnyExprPoll poll -> prPrec i 0 (concatD [prt 0 poll])
    Pml.Abs.AnyExprVarRef varref -> prPrec i 0 (concatD [prt 0 varref])
    Pml.Abs.AnyExprConst const -> prPrec i 0 (concatD [prt 0 const])
    Pml.Abs.AnyExprTimeout -> prPrec i 0 (concatD [doc (showString "timeout")])
    Pml.Abs.AnyExprNp -> prPrec i 0 (concatD [doc (showString "np_")])
    Pml.Abs.AnyExprEnabled anyexpr -> prPrec i 0 (concatD [doc (showString "enabled"), doc (showString "("), prt 0 anyexpr, doc (showString ")")])
    Pml.Abs.AnyExprPCValue anyexpr -> prPrec i 0 (concatD [doc (showString "pc_value"), doc (showString "("), prt 0 anyexpr, doc (showString ")")])
    Pml.Abs.AnyExprName name1 anyexpr name2 -> prPrec i 0 (concatD [prt 0 name1, doc (showString "["), prt 0 anyexpr, doc (showString "]"), doc (showString "@"), prt 0 name2])
    Pml.Abs.AnyExprRun name runargs runprio -> prPrec i 0 (concatD [doc (showString "run"), prt 0 name, doc (showString "("), prt 0 runargs, doc (showString ")"), prt 0 runprio])
    Pml.Abs.AnyExprGetPrio expr -> prPrec i 0 (concatD [doc (showString "get_priority"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Pml.Abs.AnyExprSetPrio expr1 expr2 -> prPrec i 0 (concatD [doc (showString "set_priority"), doc (showString "("), prt 0 expr1, doc (showString ","), prt 0 expr2, doc (showString ")")])

instance Print Pml.Abs.Ivar where
  prt i = \case
    Pml.Abs.Ivar name ivarconst ivarassign -> prPrec i 0 (concatD [prt 0 name, prt 0 ivarconst, prt 0 ivarassign])

instance Print Pml.Abs.IvarConst where
  prt i = \case
    Pml.Abs.IvarConstNone -> prPrec i 0 (concatD [])
    Pml.Abs.IvarConstOne const -> prPrec i 0 (concatD [doc (showString "["), prt 0 const, doc (showString "]")])

instance Print Pml.Abs.IvarAssign where
  prt i = \case
    Pml.Abs.IvarAssignNone -> prPrec i 0 (concatD [])
    Pml.Abs.IvarAssignAnyExpr anyexpr -> prPrec i 0 (concatD [doc (showString "="), prt 0 anyexpr])
    Pml.Abs.IvarAssignChInit chinit -> prPrec i 0 (concatD [doc (showString "="), prt 0 chinit])

instance Print Pml.Abs.ChInit where
  prt i = \case
    Pml.Abs.ChInit const chtype -> prPrec i 0 (concatD [doc (showString "["), prt 0 const, doc (showString "]"), doc (showString "of"), doc (showString "{"), prt 0 chtype, doc (showString "}")])

instance Print Pml.Abs.ChType where
  prt i = \case
    Pml.Abs.ChTypeOne typename -> prPrec i 0 (concatD [prt 0 typename])
    Pml.Abs.ChTypeCons typename chtype -> prPrec i 0 (concatD [prt 0 typename, doc (showString ","), prt 0 chtype])

instance Print Pml.Abs.VarRef where
  prt i = \case
    Pml.Abs.VarRef name varrefanyexpr varreftypedef -> prPrec i 0 (concatD [prt 0 name, prt 0 varrefanyexpr, prt 0 varreftypedef])

instance Print Pml.Abs.VarRefAnyExpr where
  prt i = \case
    Pml.Abs.VarRefAnyExprNone -> prPrec i 0 (concatD [])
    Pml.Abs.VarRefAnyExprOne anyexpr -> prPrec i 0 (concatD [doc (showString "["), prt 0 anyexpr, doc (showString "]")])

instance Print Pml.Abs.VarRefTypedef where
  prt i = \case
    Pml.Abs.VarRefTypedefNone -> prPrec i 0 (concatD [])
    Pml.Abs.VarRefTypedefOne varref -> prPrec i 0 (concatD [doc (showString "."), prt 0 varref])

instance Print Pml.Abs.Send where
  prt i = \case
    Pml.Abs.SendNormal varref sendargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "!"), prt 0 sendargs])
    Pml.Abs.SendSorted varref sendargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "!"), doc (showString "!"), prt 0 sendargs])

instance Print Pml.Abs.Receive where
  prt i = \case
    Pml.Abs.ReceiveNormal varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), prt 0 recvargs])
    Pml.Abs.ReceiveRandom varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), doc (showString "?"), prt 0 recvargs])
    Pml.Abs.ReceivePoll varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), doc (showString "<"), prt 0 recvargs, doc (showString ">")])
    Pml.Abs.ReceivePollSecond varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), doc (showString "?"), doc (showString "<"), prt 0 recvargs, doc (showString ">")])

instance Print Pml.Abs.Poll where
  prt i = \case
    Pml.Abs.PollNoSideEffect varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), doc (showString "["), prt 0 recvargs, doc (showString "]")])
    Pml.Abs.PollNoSideEffectSecond varref recvargs -> prPrec i 0 (concatD [prt 0 varref, doc (showString "?"), doc (showString "?"), doc (showString "["), prt 0 recvargs, doc (showString "]")])

instance Print Pml.Abs.SendArgs where
  prt i = \case
    Pml.Abs.SendArgs arglist -> prPrec i 0 (concatD [prt 0 arglist])
    Pml.Abs.SendArgsExpr anyexpr arglist -> prPrec i 0 (concatD [prt 0 anyexpr, doc (showString "("), prt 0 arglist, doc (showString ")")])

instance Print Pml.Abs.ArgList where
  prt i = \case
    Pml.Abs.ArgListCons anyexpr arglist -> prPrec i 0 (concatD [prt 0 anyexpr, doc (showString ","), prt 0 arglist])
    Pml.Abs.ArgListOne anyexpr -> prPrec i 0 (concatD [prt 0 anyexpr])
    Pml.Abs.ArgListNone -> prPrec i 0 (concatD [])

instance Print Pml.Abs.RecvArgs where
  prt i = \case
    Pml.Abs.RecvArgsList recvarglist -> prPrec i 0 (concatD [prt 0 recvarglist])
    Pml.Abs.RecvArgsParen recvarglist recvargs -> prPrec i 0 (concatD [prt 0 recvarglist, doc (showString "("), prt 0 recvargs, doc (showString ")")])

instance Print Pml.Abs.RecvArgList where
  prt i = \case
    Pml.Abs.RecvArgListOne recvarg -> prPrec i 0 (concatD [prt 0 recvarg])
    Pml.Abs.RecvArgListCons recvarg recvarglist -> prPrec i 0 (concatD [prt 0 recvarg, prt 0 recvarglist])

instance Print Pml.Abs.UnaryMinus where
  prt i = \case
    Pml.Abs.UnaryMinusNone -> prPrec i 0 (concatD [])
    Pml.Abs.UnaryMinusOne -> prPrec i 0 (concatD [doc (showString "-")])

instance Print Pml.Abs.RecvArg where
  prt i = \case
    Pml.Abs.RecvArgRef varref -> prPrec i 0 (concatD [prt 0 varref])
    Pml.Abs.RecvArgEval varref -> prPrec i 0 (concatD [doc (showString "eval"), doc (showString "("), prt 0 varref, doc (showString ")")])
    Pml.Abs.RecvArgConst unaryminus const -> prPrec i 0 (concatD [prt 0 unaryminus, prt 0 const])

instance Print Pml.Abs.Assign where
  prt i = \case
    Pml.Abs.AssignStd varref anyexpr -> prPrec i 0 (concatD [prt 0 varref, doc (showString "="), prt 0 anyexpr])
    Pml.Abs.AssignInc varref -> prPrec i 0 (concatD [prt 0 varref, doc (showString "+"), doc (showString "+")])
    Pml.Abs.AssignDec varref -> prPrec i 0 (concatD [prt 0 varref, doc (showString "-"), doc (showString "-")])

instance Print Pml.Abs.Pargs where
  prt i = \case
    Pml.Abs.PArgsString str -> prPrec i 0 (concatD [printString str])
    Pml.Abs.PArgsNoString arglist -> prPrec i 0 (concatD [prt 0 arglist])
    Pml.Abs.PArgsBoth str arglist -> prPrec i 0 (concatD [printString str, doc (showString ","), prt 0 arglist])

instance Print Pml.Abs.PArgList where
  prt i = \case
    Pml.Abs.PArgListNone -> prPrec i 0 (concatD [])

instance Print Pml.Abs.PargList where
  prt i = \case
    Pml.Abs.PargListOne arglist -> prPrec i 0 (concatD [doc (showString ","), prt 0 arglist])

instance Print Pml.Abs.Stmt where
  prt i = \case
    Pml.Abs.StmtIf options -> prPrec i 0 (concatD [doc (showString "if"), prt 0 options, doc (showString "fi")])
    Pml.Abs.StmtDo options -> prPrec i 0 (concatD [doc (showString "do"), prt 0 options, doc (showString "od")])
    Pml.Abs.StmtFor range sequence -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 range, doc (showString ")"), doc (showString "{"), prt 0 sequence, doc (showString "}")])
    Pml.Abs.StmtAtomic sequence -> prPrec i 0 (concatD [doc (showString "atomic"), doc (showString "{"), prt 0 sequence, doc (showString "}")])
    Pml.Abs.StmtDAtomic sequence -> prPrec i 0 (concatD [doc (showString "d_step"), doc (showString "{"), prt 0 sequence, doc (showString "}")])
    Pml.Abs.StmtSelect sequence -> prPrec i 0 (concatD [doc (showString "select"), doc (showString "("), prt 0 sequence, doc (showString ")")])
    Pml.Abs.StmtNorm sequence -> prPrec i 0 (concatD [doc (showString "{"), prt 0 sequence, doc (showString "}")])
    Pml.Abs.StmtSend send -> prPrec i 0 (concatD [prt 0 send])
    Pml.Abs.StmtRec receive -> prPrec i 0 (concatD [prt 0 receive])
    Pml.Abs.StmtAssign assign -> prPrec i 0 (concatD [prt 0 assign])
    Pml.Abs.StmtElse -> prPrec i 0 (concatD [doc (showString "else")])
    Pml.Abs.StmtBreak -> prPrec i 0 (concatD [doc (showString "break")])
    Pml.Abs.StmtGoto name -> prPrec i 0 (concatD [doc (showString "goto"), prt 0 name])
    Pml.Abs.StmtLabel name stmt -> prPrec i 0 (concatD [prt 0 name, doc (showString ":"), prt 0 stmt])
    Pml.Abs.StmtPrint printtype pargs -> prPrec i 0 (concatD [prt 0 printtype, doc (showString "("), prt 0 pargs, doc (showString ")")])
    Pml.Abs.StmtAssert expr -> prPrec i 0 (concatD [doc (showString "assert"), prt 0 expr])
    Pml.Abs.StmtCall name arglist -> prPrec i 0 (concatD [prt 0 name, doc (showString "("), prt 0 arglist, doc (showString ")")])
    Pml.Abs.StmtExpr expr -> prPrec i 0 (concatD [prt 0 expr])

instance Print Pml.Abs.Range where
  prt i = \case
    Pml.Abs.RangeIn name1 name2 -> prPrec i 0 (concatD [prt 0 name1, doc (showString "in"), prt 0 name2])
    Pml.Abs.RangeNoIn name anyexpr1 anyexpr2 -> prPrec i 0 (concatD [prt 0 name, doc (showString ":"), prt 0 anyexpr1, doc (showString ".."), prt 0 anyexpr2])

instance Print Pml.Abs.Options where
  prt i = \case
    Pml.Abs.OptionsOne sequence -> prPrec i 0 (concatD [doc (showString "::"), prt 0 sequence])
    Pml.Abs.OptionsCons sequence options -> prPrec i 0 (concatD [doc (showString "::"), prt 0 sequence, prt 0 options])

instance Print Pml.Abs.RunPrio where
  prt i = \case
    Pml.Abs.RunPrioNone -> prPrec i 0 (concatD [])
    Pml.Abs.RunPrioOne priority -> prPrec i 0 (concatD [prt 0 priority])

instance Print Pml.Abs.RunArgs where
  prt i = \case
    Pml.Abs.RunArgsNone -> prPrec i 0 (concatD [])
    Pml.Abs.RunArgsOne arglist -> prPrec i 0 (concatD [prt 0 arglist])

instance Print Pml.Abs.Expr where
  prt i = \case
    Pml.Abs.ExprAny anyexpr -> prPrec i 0 (concatD [prt 0 anyexpr])
    Pml.Abs.ExprParen expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr, doc (showString ")")])
    Pml.Abs.ExprAndOr expr1 andor expr2 -> prPrec i 0 (concatD [prt 0 expr1, prt 0 andor, prt 0 expr2])
    Pml.Abs.ExprChanPoll chanpoll varref -> prPrec i 0 (concatD [prt 0 chanpoll, doc (showString "("), prt 0 varref, doc (showString ")")])

instance Print Pml.Abs.Uname where
  prt i = \case
    Pml.Abs.Uname name -> prPrec i 0 (concatD [prt 0 name])

instance Print Pml.Abs.Name where
  prt i = \case
    Pml.Abs.Name pident -> prPrec i 0 (concatD [prt 0 pident])
