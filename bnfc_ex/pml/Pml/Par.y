-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Pml.Par
  ( happyError
  , myLexer
  , pListModule
  , pVisible
  , pChanPoll
  , pSeparator
  , pTypename
  , pAndOr
  , pBinOp
  , pUnrOp
  , pConst
  , pPrintType
  , pModule
  , pProctype
  , pInline
  , pPactive
  , pPdeclList
  , pPpriority
  , pPenabler
  , pInit
  , pIpriority
  , pNever
  , pTrace
  , pUtype
  , pMtype
  , pMsep
  , pMequals
  , pMname
  , pDeclList
  , pDecl
  , pDclIvar
  , pDeclVisible
  , pUnsignedDecl
  , pUDclAssign
  , pActive
  , pAConst
  , pPriority
  , pEnabler
  , pSequence
  , pUStmt
  , pStep
  , pVarRefList
  , pAnyExpr
  , pIvar
  , pIvarConst
  , pIvarAssign
  , pChInit
  , pChType
  , pVarRef
  , pVarRefAnyExpr
  , pVarRefTypedef
  , pSend
  , pReceive
  , pPoll
  , pSendArgs
  , pArgList
  , pRecvArgs
  , pRecvArgList
  , pUnaryMinus
  , pRecvArg
  , pAssign
  , pPargs
  , pPArgList
  , pPargList
  , pStmt
  , pRange
  , pOptions
  , pRunPrio
  , pRunArgs
  , pExpr
  , pUname
  , pName
  ) where

import Prelude

import qualified Pml.Abs
import Pml.Lex

}

%name pListModule ListModule
%name pVisible Visible
%name pChanPoll ChanPoll
%name pSeparator Separator
%name pTypename Typename
%name pAndOr AndOr
%name pBinOp BinOp
%name pUnrOp UnrOp
%name pConst Const
%name pPrintType PrintType
%name pModule Module
%name pProctype Proctype
%name pInline Inline
%name pPactive Pactive
%name pPdeclList PdeclList
%name pPpriority Ppriority
%name pPenabler Penabler
%name pInit Init
%name pIpriority Ipriority
%name pNever Never
%name pTrace Trace
%name pUtype Utype
%name pMtype Mtype
%name pMsep Msep
%name pMequals Mequals
%name pMname Mname
%name pDeclList DeclList
%name pDecl Decl
%name pDclIvar DclIvar
%name pDeclVisible DeclVisible
%name pUnsignedDecl UnsignedDecl
%name pUDclAssign UDclAssign
%name pActive Active
%name pAConst AConst
%name pPriority Priority
%name pEnabler Enabler
%name pSequence Sequence
%name pUStmt UStmt
%name pStep Step
%name pVarRefList VarRefList
%name pAnyExpr AnyExpr
%name pIvar Ivar
%name pIvarConst IvarConst
%name pIvarAssign IvarAssign
%name pChInit ChInit
%name pChType ChType
%name pVarRef VarRef
%name pVarRefAnyExpr VarRefAnyExpr
%name pVarRefTypedef VarRefTypedef
%name pSend Send
%name pReceive Receive
%name pPoll Poll
%name pSendArgs SendArgs
%name pArgList ArgList
%name pRecvArgs RecvArgs
%name pRecvArgList RecvArgList
%name pUnaryMinus UnaryMinus
%name pRecvArg RecvArg
%name pAssign Assign
%name pPargs Pargs
%name pPArgList PArgList
%name pPargList PargList
%name pStmt Stmt
%name pRange Range
%name pOptions Options
%name pRunPrio RunPrio
%name pRunArgs RunArgs
%name pExpr Expr
%name pUname Uname
%name pName Name
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'            { PT _ (TS _ 1)      }
  '!='           { PT _ (TS _ 2)      }
  '%'            { PT _ (TS _ 3)      }
  '&'            { PT _ (TS _ 4)      }
  '&&'           { PT _ (TS _ 5)      }
  '('            { PT _ (TS _ 6)      }
  ')'            { PT _ (TS _ 7)      }
  '*'            { PT _ (TS _ 8)      }
  '+'            { PT _ (TS _ 9)      }
  ','            { PT _ (TS _ 10)     }
  '-'            { PT _ (TS _ 11)     }
  '->'           { PT _ (TS _ 12)     }
  '.'            { PT _ (TS _ 13)     }
  '..'           { PT _ (TS _ 14)     }
  '/'            { PT _ (TS _ 15)     }
  ':'            { PT _ (TS _ 16)     }
  '::'           { PT _ (TS _ 17)     }
  ';'            { PT _ (TS _ 18)     }
  '<'            { PT _ (TS _ 19)     }
  '<<'           { PT _ (TS _ 20)     }
  '<='           { PT _ (TS _ 21)     }
  '='            { PT _ (TS _ 22)     }
  '=='           { PT _ (TS _ 23)     }
  '>'            { PT _ (TS _ 24)     }
  '>='           { PT _ (TS _ 25)     }
  '>>'           { PT _ (TS _ 26)     }
  '?'            { PT _ (TS _ 27)     }
  '@'            { PT _ (TS _ 28)     }
  '['            { PT _ (TS _ 29)     }
  ']'            { PT _ (TS _ 30)     }
  '^'            { PT _ (TS _ 31)     }
  'active'       { PT _ (TS _ 32)     }
  'assert'       { PT _ (TS _ 33)     }
  'atomic'       { PT _ (TS _ 34)     }
  'bit'          { PT _ (TS _ 35)     }
  'bool'         { PT _ (TS _ 36)     }
  'break'        { PT _ (TS _ 37)     }
  'byte'         { PT _ (TS _ 38)     }
  'chan'         { PT _ (TS _ 39)     }
  'd_step'       { PT _ (TS _ 40)     }
  'do'           { PT _ (TS _ 41)     }
  'else'         { PT _ (TS _ 42)     }
  'empty'        { PT _ (TS _ 43)     }
  'enabled'      { PT _ (TS _ 44)     }
  'eval'         { PT _ (TS _ 45)     }
  'false'        { PT _ (TS _ 46)     }
  'fi'           { PT _ (TS _ 47)     }
  'for'          { PT _ (TS _ 48)     }
  'full'         { PT _ (TS _ 49)     }
  'get_priority' { PT _ (TS _ 50)     }
  'goto'         { PT _ (TS _ 51)     }
  'hidden'       { PT _ (TS _ 52)     }
  'if'           { PT _ (TS _ 53)     }
  'in'           { PT _ (TS _ 54)     }
  'init'         { PT _ (TS _ 55)     }
  'inline'       { PT _ (TS _ 56)     }
  'int'          { PT _ (TS _ 57)     }
  'len'          { PT _ (TS _ 58)     }
  'mtype'        { PT _ (TS _ 59)     }
  'nempty'       { PT _ (TS _ 60)     }
  'never'        { PT _ (TS _ 61)     }
  'nfull'        { PT _ (TS _ 62)     }
  'np_'          { PT _ (TS _ 63)     }
  'od'           { PT _ (TS _ 64)     }
  'of'           { PT _ (TS _ 65)     }
  'pc_value'     { PT _ (TS _ 66)     }
  'print'        { PT _ (TS _ 67)     }
  'printf'       { PT _ (TS _ 68)     }
  'printm'       { PT _ (TS _ 69)     }
  'priority'     { PT _ (TS _ 70)     }
  'proctype'     { PT _ (TS _ 71)     }
  'provided'     { PT _ (TS _ 72)     }
  'run'          { PT _ (TS _ 73)     }
  'select'       { PT _ (TS _ 74)     }
  'set_priority' { PT _ (TS _ 75)     }
  'short'        { PT _ (TS _ 76)     }
  'show'         { PT _ (TS _ 77)     }
  'skip'         { PT _ (TS _ 78)     }
  'timeout'      { PT _ (TS _ 79)     }
  'trace'        { PT _ (TS _ 80)     }
  'true'         { PT _ (TS _ 81)     }
  'typedef'      { PT _ (TS _ 82)     }
  'unless'       { PT _ (TS _ 83)     }
  'unsigned'     { PT _ (TS _ 84)     }
  'xr'           { PT _ (TS _ 85)     }
  'xs'           { PT _ (TS _ 86)     }
  '{'            { PT _ (TS _ 87)     }
  '|'            { PT _ (TS _ 88)     }
  '||'           { PT _ (TS _ 89)     }
  '}'            { PT _ (TS _ 90)     }
  '~'            { PT _ (TS _ 91)     }
  L_integ        { PT _ (TI $$)       }
  L_quoted       { PT _ (TL $$)       }
  L_PIdent       { PT _ (T_PIdent $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

PIdent :: { Pml.Abs.PIdent }
PIdent  : L_PIdent { Pml.Abs.PIdent $1 }

ListModule :: { [Pml.Abs.Module] }
ListModule : Module { (:[]) $1 } | Module ListModule { (:) $1 $2 }

Visible :: { Pml.Abs.Visible }
Visible
  : 'hidden' { Pml.Abs.Visible_hidden }
  | 'show' { Pml.Abs.Visible_show }

ChanPoll :: { Pml.Abs.ChanPoll }
ChanPoll
  : 'full' { Pml.Abs.ChanPoll_full }
  | 'empty' 'nfull' 'nempty' { Pml.Abs.ChanPoll1 }

Separator :: { Pml.Abs.Separator }
Separator
  : ';' { Pml.Abs.Separator1 } | '->' { Pml.Abs.Separator2 }

Typename :: { Pml.Abs.Typename }
Typename
  : 'bit' { Pml.Abs.Typename_bit }
  | 'bool' { Pml.Abs.Typename_bool }
  | 'byte' { Pml.Abs.Typename_byte }
  | 'short' { Pml.Abs.Typename_short }
  | 'int' { Pml.Abs.Typename_int }
  | 'mtype' { Pml.Abs.Typename_mtype }
  | 'chan' { Pml.Abs.Typename_chan }
  | Uname { Pml.Abs.TypenameUname $1 }

AndOr :: { Pml.Abs.AndOr }
AndOr : '&&' { Pml.Abs.AndOr1 } | '||' { Pml.Abs.AndOr2 }

BinOp :: { Pml.Abs.BinOp }
BinOp
  : '+' { Pml.Abs.BinOp1 }
  | '-' { Pml.Abs.BinOp2 }
  | '*' { Pml.Abs.BinOp3 }
  | '/' { Pml.Abs.BinOp4 }
  | '%' { Pml.Abs.BinOp5 }
  | '&' { Pml.Abs.BinOp6 }
  | '^' { Pml.Abs.BinOp7 }
  | '|' { Pml.Abs.BinOp8 }
  | '>' { Pml.Abs.BinOp9 }
  | '<' { Pml.Abs.BinOp10 }
  | '>=' { Pml.Abs.BinOp11 }
  | '<=' { Pml.Abs.BinOp12 }
  | '==' { Pml.Abs.BinOp13 }
  | '!=' { Pml.Abs.BinOp14 }
  | '<<' { Pml.Abs.BinOp15 }
  | '>>' { Pml.Abs.BinOp16 }
  | AndOr { Pml.Abs.BinOpAndOr $1 }

UnrOp :: { Pml.Abs.UnrOp }
UnrOp
  : '~' { Pml.Abs.UnrOp1 }
  | '-' { Pml.Abs.UnrOp2 }
  | '!' { Pml.Abs.UnrOp3 }

Const :: { Pml.Abs.Const }
Const
  : 'true' { Pml.Abs.Const_true }
  | 'false' { Pml.Abs.Const_false }
  | 'skip' { Pml.Abs.Const_skip }
  | Integer { Pml.Abs.ConstInteger $1 }

PrintType :: { Pml.Abs.PrintType }
PrintType
  : 'print' { Pml.Abs.PrintType_print }
  | 'printf' { Pml.Abs.PrintType_printf }
  | 'printm' { Pml.Abs.PrintType_printm }

Module :: { Pml.Abs.Module }
Module
  : Proctype { Pml.Abs.Mproc $1 }
  | Inline { Pml.Abs.Minline $1 }
  | Init { Pml.Abs.Minit $1 }
  | Never { Pml.Abs.Mnever $1 }
  | Trace { Pml.Abs.Mtrace $1 }
  | Utype { Pml.Abs.Mutype $1 }
  | Mtype { Pml.Abs.Mmtype $1 }
  | DeclList { Pml.Abs.MdeclList $1 }

Proctype :: { Pml.Abs.Proctype }
Proctype
  : Pactive 'proctype' Name '(' PdeclList ')' Ppriority Penabler '{' Sequence '}' { Pml.Abs.Ptype $1 $3 $5 $7 $8 $10 }

Inline :: { Pml.Abs.Inline }
Inline
  : 'inline' Name '(' ArgList ')' '{' Sequence '}' { Pml.Abs.Iline $2 $4 $7 }

Pactive :: { Pml.Abs.Pactive }
Pactive
  : {- empty -} { Pml.Abs.PactiveNone }
  | Active { Pml.Abs.PactiveOne $1 }

PdeclList :: { Pml.Abs.PdeclList }
PdeclList
  : {- empty -} { Pml.Abs.PdeclListNone }
  | DeclList { Pml.Abs.PdeclListOne $1 }

Ppriority :: { Pml.Abs.Ppriority }
Ppriority
  : {- empty -} { Pml.Abs.PpriorityNone }
  | Priority { Pml.Abs.PpriorityOne $1 }

Penabler :: { Pml.Abs.Penabler }
Penabler
  : {- empty -} { Pml.Abs.PenablerNone }
  | Enabler { Pml.Abs.PenablerOne $1 }

Init :: { Pml.Abs.Init }
Init
  : 'init' Ipriority '{' Sequence '}' ';' { Pml.Abs.Initialise $2 $4 }

Ipriority :: { Pml.Abs.Ipriority }
Ipriority
  : {- empty -} { Pml.Abs.IpriorityNone }
  | Priority { Pml.Abs.IpriorityOne $1 }

Never :: { Pml.Abs.Never }
Never : 'never' '{' Sequence '}' { Pml.Abs.Nvr $3 }

Trace :: { Pml.Abs.Trace }
Trace : 'trace' '{' Sequence '}' { Pml.Abs.Trc $3 }

Utype :: { Pml.Abs.Utype }
Utype : 'typedef' Name '{' DeclList '}' ';' { Pml.Abs.Utp $2 $4 }

Mtype :: { Pml.Abs.Mtype }
Mtype : 'mtype' Mequals '{' Mname '}' Msep { Pml.Abs.Mtp $2 $4 $6 }

Msep :: { Pml.Abs.Msep }
Msep : {- empty -} { Pml.Abs.MsepNone } | ';' { Pml.Abs.MsepOne }

Mequals :: { Pml.Abs.Mequals }
Mequals : {- empty -} { Pml.Abs.Meq } | '=' { Pml.Abs.Meq }

Mname :: { Pml.Abs.Mname }
Mname
  : Name { Pml.Abs.MnameOne $1 }
  | Name ',' Mname { Pml.Abs.Mnamecons $1 $3 }

DeclList :: { Pml.Abs.DeclList }
DeclList
  : Decl Separator { Pml.Abs.DclListOne $1 $2 }
  | Decl { Pml.Abs.DclListOneNoSep $1 }
  | Decl Separator DeclList { Pml.Abs.DclListCons $1 $2 $3 }

Decl :: { Pml.Abs.Decl }
Decl
  : DeclVisible Typename DclIvar { Pml.Abs.DclOne $1 $2 $3 }
  | DeclVisible UnsignedDecl { Pml.Abs.DclOneUnsigned $1 $2 }

DclIvar :: { Pml.Abs.DclIvar }
DclIvar
  : Ivar ',' DclIvar { Pml.Abs.DclIvarCons $1 $3 }
  | Ivar { Pml.Abs.DclIvarSub $1 }

DeclVisible :: { Pml.Abs.DeclVisible }
DeclVisible
  : {- empty -} { Pml.Abs.DclVisNone }
  | Visible { Pml.Abs.DclVisOne $1 }

UnsignedDecl :: { Pml.Abs.UnsignedDecl }
UnsignedDecl
  : 'unsigned' Name ':' Const UDclAssign { Pml.Abs.UDcl $2 $4 $5 }

UDclAssign :: { Pml.Abs.UDclAssign }
UDclAssign
  : {- empty -} { Pml.Abs.UDclAssignNone }
  | '=' AnyExpr { Pml.Abs.UdclAssignOne $2 }

Active :: { Pml.Abs.Active }
Active : 'active' AConst { Pml.Abs.Active $2 }

AConst :: { Pml.Abs.AConst }
AConst
  : {- empty -} { Pml.Abs.AconstNone }
  | '[' Const ']' { Pml.Abs.AconstOne $2 }

Priority :: { Pml.Abs.Priority }
Priority : 'priority' Const { Pml.Abs.Priority $2 }

Enabler :: { Pml.Abs.Enabler }
Enabler : 'provided' '(' Expr ')' { Pml.Abs.Enabler $3 }

Sequence :: { Pml.Abs.Sequence }
Sequence
  : Step { Pml.Abs.SeqOne $1 }
  | Step Separator { Pml.Abs.SeqOneSep $1 $2 }
  | Step Sequence { Pml.Abs.SeqNoStep $1 $2 }
  | Step Separator Sequence { Pml.Abs.SeqCons $1 $2 $3 }

UStmt :: { Pml.Abs.UStmt }
UStmt
  : {- empty -} { Pml.Abs.UStmtNone }
  | 'unless' Stmt { Pml.Abs.UStmtOne $2 }

Step :: { Pml.Abs.Step }
Step
  : Mtype { Pml.Abs.StepMType $1 }
  | Stmt UStmt { Pml.Abs.StepStmt $1 $2 }
  | DeclList { Pml.Abs.StepDclList $1 }
  | 'xr' VarRefList { Pml.Abs.StepXR $2 }
  | 'xs' VarRefList { Pml.Abs.StepXS $2 }

VarRefList :: { Pml.Abs.VarRefList }
VarRefList
  : VarRef { Pml.Abs.VarRefListOne $1 }
  | VarRef ',' VarRefList { Pml.Abs.VarRefListCons $1 $3 }

AnyExpr :: { Pml.Abs.AnyExpr }
AnyExpr
  : '(' AnyExpr ')' { Pml.Abs.AnyExprParen $2 }
  | AnyExpr BinOp AnyExpr { Pml.Abs.AnyExprBinOp $1 $2 $3 }
  | UnrOp AnyExpr { Pml.Abs.AnyExprUnrOp $1 $2 }
  | '(' AnyExpr '->' AnyExpr ':' AnyExpr ')' { Pml.Abs.AnyExprCond $2 $4 $6 }
  | 'len' '(' VarRef ')' { Pml.Abs.AnyExprLen $3 }
  | Poll { Pml.Abs.AnyExprPoll $1 }
  | VarRef { Pml.Abs.AnyExprVarRef $1 }
  | Const { Pml.Abs.AnyExprConst $1 }
  | 'timeout' { Pml.Abs.AnyExprTimeout }
  | 'np_' { Pml.Abs.AnyExprNp }
  | 'enabled' '(' AnyExpr ')' { Pml.Abs.AnyExprEnabled $3 }
  | 'pc_value' '(' AnyExpr ')' { Pml.Abs.AnyExprPCValue $3 }
  | Name '[' AnyExpr ']' '@' Name { Pml.Abs.AnyExprName $1 $3 $6 }
  | 'run' Name '(' RunArgs ')' RunPrio { Pml.Abs.AnyExprRun $2 $4 $6 }
  | 'get_priority' '(' Expr ')' { Pml.Abs.AnyExprGetPrio $3 }
  | 'set_priority' '(' Expr ',' Expr ')' { Pml.Abs.AnyExprSetPrio $3 $5 }

Ivar :: { Pml.Abs.Ivar }
Ivar : Name IvarConst IvarAssign { Pml.Abs.Ivar $1 $2 $3 }

IvarConst :: { Pml.Abs.IvarConst }
IvarConst
  : {- empty -} { Pml.Abs.IvarConstNone }
  | '[' Const ']' { Pml.Abs.IvarConstOne $2 }

IvarAssign :: { Pml.Abs.IvarAssign }
IvarAssign
  : {- empty -} { Pml.Abs.IvarAssignNone }
  | '=' AnyExpr { Pml.Abs.IvarAssignAnyExpr $2 }
  | '=' ChInit { Pml.Abs.IvarAssignChInit $2 }

ChInit :: { Pml.Abs.ChInit }
ChInit : '[' Const ']' 'of' '{' ChType '}' { Pml.Abs.ChInit $2 $6 }

ChType :: { Pml.Abs.ChType }
ChType
  : Typename { Pml.Abs.ChTypeOne $1 }
  | Typename ',' ChType { Pml.Abs.ChTypeCons $1 $3 }

VarRef :: { Pml.Abs.VarRef }
VarRef
  : Name VarRefAnyExpr VarRefTypedef { Pml.Abs.VarRef $1 $2 $3 }

VarRefAnyExpr :: { Pml.Abs.VarRefAnyExpr }
VarRefAnyExpr
  : {- empty -} { Pml.Abs.VarRefAnyExprNone }
  | '[' AnyExpr ']' { Pml.Abs.VarRefAnyExprOne $2 }

VarRefTypedef :: { Pml.Abs.VarRefTypedef }
VarRefTypedef
  : {- empty -} { Pml.Abs.VarRefTypedefNone }
  | '.' VarRef { Pml.Abs.VarRefTypedefOne $2 }

Send :: { Pml.Abs.Send }
Send
  : VarRef '!' SendArgs { Pml.Abs.SendNormal $1 $3 }
  | VarRef '!' '!' SendArgs { Pml.Abs.SendSorted $1 $4 }

Receive :: { Pml.Abs.Receive }
Receive
  : VarRef '?' RecvArgs { Pml.Abs.ReceiveNormal $1 $3 }
  | VarRef '?' '?' RecvArgs { Pml.Abs.ReceiveRandom $1 $4 }
  | VarRef '?' '<' RecvArgs '>' { Pml.Abs.ReceivePoll $1 $4 }
  | VarRef '?' '?' '<' RecvArgs '>' { Pml.Abs.ReceivePollSecond $1 $5 }

Poll :: { Pml.Abs.Poll }
Poll
  : VarRef '?' '[' RecvArgs ']' { Pml.Abs.PollNoSideEffect $1 $4 }
  | VarRef '?' '?' '[' RecvArgs ']' { Pml.Abs.PollNoSideEffectSecond $1 $5 }

SendArgs :: { Pml.Abs.SendArgs }
SendArgs
  : ArgList { Pml.Abs.SendArgs $1 }
  | AnyExpr '(' ArgList ')' { Pml.Abs.SendArgsExpr $1 $3 }

ArgList :: { Pml.Abs.ArgList }
ArgList
  : AnyExpr ',' ArgList { Pml.Abs.ArgListCons $1 $3 }
  | AnyExpr { Pml.Abs.ArgListOne $1 }
  | {- empty -} { Pml.Abs.ArgListNone }

RecvArgs :: { Pml.Abs.RecvArgs }
RecvArgs
  : RecvArgList { Pml.Abs.RecvArgsList $1 }
  | RecvArgList '(' RecvArgs ')' { Pml.Abs.RecvArgsParen $1 $3 }

RecvArgList :: { Pml.Abs.RecvArgList }
RecvArgList
  : RecvArg { Pml.Abs.RecvArgListOne $1 }
  | RecvArg RecvArgList { Pml.Abs.RecvArgListCons $1 $2 }

UnaryMinus :: { Pml.Abs.UnaryMinus }
UnaryMinus
  : {- empty -} { Pml.Abs.UnaryMinusNone }
  | '-' { Pml.Abs.UnaryMinusOne }

RecvArg :: { Pml.Abs.RecvArg }
RecvArg
  : VarRef { Pml.Abs.RecvArgRef $1 }
  | 'eval' '(' VarRef ')' { Pml.Abs.RecvArgEval $3 }
  | UnaryMinus Const { Pml.Abs.RecvArgConst $1 $2 }

Assign :: { Pml.Abs.Assign }
Assign
  : VarRef '=' AnyExpr { Pml.Abs.AssignStd $1 $3 }
  | VarRef '+' '+' { Pml.Abs.AssignInc $1 }
  | VarRef '-' '-' { Pml.Abs.AssignDec $1 }

Pargs :: { Pml.Abs.Pargs }
Pargs
  : String { Pml.Abs.PArgsString $1 }
  | ArgList { Pml.Abs.PArgsNoString $1 }
  | String ',' ArgList { Pml.Abs.PArgsBoth $1 $3 }

PArgList :: { Pml.Abs.PArgList }
PArgList : {- empty -} { Pml.Abs.PArgListNone }

PargList :: { Pml.Abs.PargList }
PargList : ',' ArgList { Pml.Abs.PargListOne $2 }

Stmt :: { Pml.Abs.Stmt }
Stmt
  : 'if' Options 'fi' { Pml.Abs.StmtIf $2 }
  | 'do' Options 'od' { Pml.Abs.StmtDo $2 }
  | 'for' '(' Range ')' '{' Sequence '}' { Pml.Abs.StmtFor $3 $6 }
  | 'atomic' '{' Sequence '}' { Pml.Abs.StmtAtomic $3 }
  | 'd_step' '{' Sequence '}' { Pml.Abs.StmtDAtomic $3 }
  | 'select' '(' Sequence ')' { Pml.Abs.StmtSelect $3 }
  | '{' Sequence '}' { Pml.Abs.StmtNorm $2 }
  | Send { Pml.Abs.StmtSend $1 }
  | Receive { Pml.Abs.StmtRec $1 }
  | Assign { Pml.Abs.StmtAssign $1 }
  | 'else' { Pml.Abs.StmtElse }
  | 'break' { Pml.Abs.StmtBreak }
  | 'goto' Name { Pml.Abs.StmtGoto $2 }
  | Name ':' Stmt { Pml.Abs.StmtLabel $1 $3 }
  | PrintType '(' Pargs ')' { Pml.Abs.StmtPrint $1 $3 }
  | 'assert' Expr { Pml.Abs.StmtAssert $2 }
  | Name '(' ArgList ')' { Pml.Abs.StmtCall $1 $3 }
  | Expr { Pml.Abs.StmtExpr $1 }

Range :: { Pml.Abs.Range }
Range
  : Name 'in' Name { Pml.Abs.RangeIn $1 $3 }
  | Name ':' AnyExpr '..' AnyExpr { Pml.Abs.RangeNoIn $1 $3 $5 }

Options :: { Pml.Abs.Options }
Options
  : '::' Sequence { Pml.Abs.OptionsOne $2 }
  | '::' Sequence Options { Pml.Abs.OptionsCons $2 $3 }

RunPrio :: { Pml.Abs.RunPrio }
RunPrio
  : {- empty -} { Pml.Abs.RunPrioNone }
  | Priority { Pml.Abs.RunPrioOne $1 }

RunArgs :: { Pml.Abs.RunArgs }
RunArgs
  : {- empty -} { Pml.Abs.RunArgsNone }
  | ArgList { Pml.Abs.RunArgsOne $1 }

Expr :: { Pml.Abs.Expr }
Expr
  : AnyExpr { Pml.Abs.ExprAny $1 }
  | '(' Expr ')' { Pml.Abs.ExprParen $2 }
  | Expr AndOr Expr { Pml.Abs.ExprAndOr $1 $2 $3 }
  | ChanPoll '(' VarRef ')' { Pml.Abs.ExprChanPoll $1 $3 }

Uname :: { Pml.Abs.Uname }
Uname : Name { Pml.Abs.Uname $1 }

Name :: { Pml.Abs.Name }
Name : PIdent { Pml.Abs.Name $1 }

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

