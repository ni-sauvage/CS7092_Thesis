-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Pml.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Pml.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transPIdent :: Pml.Abs.PIdent -> Result
transPIdent x = case x of
  Pml.Abs.PIdent string -> failure x

transVisible :: Pml.Abs.Visible -> Result
transVisible x = case x of
  Pml.Abs.Visible_hidden -> failure x
  Pml.Abs.Visible_show -> failure x

transChanPoll :: Pml.Abs.ChanPoll -> Result
transChanPoll x = case x of
  Pml.Abs.ChanPoll_full -> failure x
  Pml.Abs.ChanPoll1 -> failure x

transSeparator :: Pml.Abs.Separator -> Result
transSeparator x = case x of
  Pml.Abs.Separator1 -> failure x
  Pml.Abs.Separator2 -> failure x

transTypename :: Pml.Abs.Typename -> Result
transTypename x = case x of
  Pml.Abs.Typename_bit -> failure x
  Pml.Abs.Typename_bool -> failure x
  Pml.Abs.Typename_byte -> failure x
  Pml.Abs.Typename_short -> failure x
  Pml.Abs.Typename_int -> failure x
  Pml.Abs.Typename_mtype -> failure x
  Pml.Abs.Typename_chan -> failure x
  Pml.Abs.TypenamePIdent pident -> failure x

transUnrOp :: Pml.Abs.UnrOp -> Result
transUnrOp x = case x of
  Pml.Abs.UnrOp1 -> failure x
  Pml.Abs.UnrOp2 -> failure x
  Pml.Abs.UnrOp3 -> failure x

transConst :: Pml.Abs.Const -> Result
transConst x = case x of
  Pml.Abs.Const_true -> failure x
  Pml.Abs.Const_false -> failure x
  Pml.Abs.Const_skip -> failure x
  Pml.Abs.ConstInteger integer -> failure x

transPrintType :: Pml.Abs.PrintType -> Result
transPrintType x = case x of
  Pml.Abs.PrintType_print -> failure x
  Pml.Abs.PrintType_printf -> failure x
  Pml.Abs.PrintType_printm -> failure x

transModule :: Pml.Abs.Module -> Result
transModule x = case x of
  Pml.Abs.Mproc proctype -> failure x
  Pml.Abs.Minline inline -> failure x
  Pml.Abs.Minit init -> failure x
  Pml.Abs.Mnever never -> failure x
  Pml.Abs.Mtrace trace -> failure x
  Pml.Abs.Mutype utype -> failure x
  Pml.Abs.Mmtype mtype -> failure x
  Pml.Abs.MdeclList decllist -> failure x

transProctype :: Pml.Abs.Proctype -> Result
transProctype x = case x of
  Pml.Abs.Ptype pactive pident pdecllist ppriority penabler sequence -> failure x

transInline :: Pml.Abs.Inline -> Result
transInline x = case x of
  Pml.Abs.Iline pident anyexprs sequence -> failure x

transPactive :: Pml.Abs.Pactive -> Result
transPactive x = case x of
  Pml.Abs.PactiveNone -> failure x
  Pml.Abs.PactiveOne active -> failure x

transPdeclList :: Pml.Abs.PdeclList -> Result
transPdeclList x = case x of
  Pml.Abs.PdeclListNone -> failure x
  Pml.Abs.PdeclListOne decllist -> failure x

transPpriority :: Pml.Abs.Ppriority -> Result
transPpriority x = case x of
  Pml.Abs.PpriorityNone -> failure x
  Pml.Abs.PpriorityOne priority -> failure x

transPenabler :: Pml.Abs.Penabler -> Result
transPenabler x = case x of
  Pml.Abs.PenablerNone -> failure x
  Pml.Abs.PenablerOne enabler -> failure x

transInit :: Pml.Abs.Init -> Result
transInit x = case x of
  Pml.Abs.Initialise ipriority sequence -> failure x

transIpriority :: Pml.Abs.Ipriority -> Result
transIpriority x = case x of
  Pml.Abs.IpriorityNone -> failure x
  Pml.Abs.IpriorityOne priority -> failure x

transNever :: Pml.Abs.Never -> Result
transNever x = case x of
  Pml.Abs.Nvr sequence -> failure x

transTrace :: Pml.Abs.Trace -> Result
transTrace x = case x of
  Pml.Abs.Trc sequence -> failure x

transUtype :: Pml.Abs.Utype -> Result
transUtype x = case x of
  Pml.Abs.Utp pident decllist -> failure x

transMtype :: Pml.Abs.Mtype -> Result
transMtype x = case x of
  Pml.Abs.MtpEq mequals pidents msep -> failure x
  Pml.Abs.MtpNoEq pidents msep -> failure x

transMsep :: Pml.Abs.Msep -> Result
transMsep x = case x of
  Pml.Abs.MsepNone -> failure x
  Pml.Abs.MsepOne -> failure x

transMequals :: Pml.Abs.Mequals -> Result
transMequals x = case x of
  Pml.Abs.Meq -> failure x

transDeclList :: Pml.Abs.DeclList -> Result
transDeclList x = case x of
  Pml.Abs.DclListOne decl separator -> failure x
  Pml.Abs.DclListOneNoSep decl -> failure x
  Pml.Abs.DclListCons decl separator decllist -> failure x

transDecl :: Pml.Abs.Decl -> Result
transDecl x = case x of
  Pml.Abs.DclOne declvisible typename ivars -> failure x
  Pml.Abs.DclOneUnsigned declvisible unsigneddecl -> failure x

transDeclVisible :: Pml.Abs.DeclVisible -> Result
transDeclVisible x = case x of
  Pml.Abs.DclVisNone -> failure x
  Pml.Abs.DclVisOne visible -> failure x

transUnsignedDecl :: Pml.Abs.UnsignedDecl -> Result
transUnsignedDecl x = case x of
  Pml.Abs.UDcl pident const udclassign -> failure x

transUDclAssign :: Pml.Abs.UDclAssign -> Result
transUDclAssign x = case x of
  Pml.Abs.UDclAssignNone -> failure x
  Pml.Abs.UdclAssignOne anyexpr -> failure x

transActive :: Pml.Abs.Active -> Result
transActive x = case x of
  Pml.Abs.Active aconst -> failure x

transAConst :: Pml.Abs.AConst -> Result
transAConst x = case x of
  Pml.Abs.AconstNone -> failure x
  Pml.Abs.AconstOne const -> failure x

transPriority :: Pml.Abs.Priority -> Result
transPriority x = case x of
  Pml.Abs.Priority const -> failure x

transEnabler :: Pml.Abs.Enabler -> Result
transEnabler x = case x of
  Pml.Abs.Enabler expr -> failure x

transSequence :: Pml.Abs.Sequence -> Result
transSequence x = case x of
  Pml.Abs.SeqOne step -> failure x
  Pml.Abs.SeqOneSep step separator -> failure x
  Pml.Abs.SeqNoStep step sequence -> failure x
  Pml.Abs.SeqCons step separator sequence -> failure x

transUStmt :: Pml.Abs.UStmt -> Result
transUStmt x = case x of
  Pml.Abs.UStmtNone -> failure x
  Pml.Abs.UStmtOne stmt -> failure x

transStep :: Pml.Abs.Step -> Result
transStep x = case x of
  Pml.Abs.StepMType mtype -> failure x
  Pml.Abs.StepStmt stmt ustmt -> failure x
  Pml.Abs.StepDclList decllist -> failure x
  Pml.Abs.StepXR varreflist -> failure x
  Pml.Abs.StepXS varreflist -> failure x

transVarRefList :: Pml.Abs.VarRefList -> Result
transVarRefList x = case x of
  Pml.Abs.VarRefListOne varref -> failure x
  Pml.Abs.VarRefListCons varref varreflist -> failure x

transAnyExpr :: Pml.Abs.AnyExpr -> Result
transAnyExpr x = case x of
  Pml.Abs.AnyExprCond anyexpr1 anyexpr2 anyexpr3 -> failure x
  Pml.Abs.AnyExprlor anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprland anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprbitor anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprbitxor anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprbitand anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExpreq anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprneq anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprlthan anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprgrthan anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprle anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprge anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprleft anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprright anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprplus anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprminus anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprtimes anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprdiv anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprmod anyexpr1 anyexpr2 -> failure x
  Pml.Abs.AnyExprUnrOp unrop anyexpr -> failure x
  Pml.Abs.AnyExprLen varref -> failure x
  Pml.Abs.AnyExprPoll poll -> failure x
  Pml.Abs.AnyExprVarRef varref -> failure x
  Pml.Abs.AnyExprConst const -> failure x
  Pml.Abs.AnyExprTimeout -> failure x
  Pml.Abs.AnyExprNp -> failure x
  Pml.Abs.AnyExprEnabled anyexpr -> failure x
  Pml.Abs.AnyExprPCValue anyexpr -> failure x
  Pml.Abs.AnyExprName pident1 anyexpr pident2 -> failure x
  Pml.Abs.AnyExprRun pident runargs runprio -> failure x
  Pml.Abs.AnyExprGetPrio expr -> failure x
  Pml.Abs.AnyExprSetPrio expr1 expr2 -> failure x

transIvar :: Pml.Abs.Ivar -> Result
transIvar x = case x of
  Pml.Abs.Ivar pident ivarconst ivarassign -> failure x

transIvarConst :: Pml.Abs.IvarConst -> Result
transIvarConst x = case x of
  Pml.Abs.IvarConstNone -> failure x
  Pml.Abs.IvarConstOne const -> failure x

transIvarAssign :: Pml.Abs.IvarAssign -> Result
transIvarAssign x = case x of
  Pml.Abs.IvarAssignNone -> failure x
  Pml.Abs.IvarAssignAnyExpr anyexpr -> failure x
  Pml.Abs.IvarAssignChInit chinit -> failure x

transChInit :: Pml.Abs.ChInit -> Result
transChInit x = case x of
  Pml.Abs.ChInit const typenames -> failure x

transVarRef :: Pml.Abs.VarRef -> Result
transVarRef x = case x of
  Pml.Abs.VarRef pident varrefanyexpr varreftypedef -> failure x

transVarRefAnyExpr :: Pml.Abs.VarRefAnyExpr -> Result
transVarRefAnyExpr x = case x of
  Pml.Abs.VarRefAnyExprNone -> failure x
  Pml.Abs.VarRefAnyExprOne anyexpr -> failure x

transVarRefTypedef :: Pml.Abs.VarRefTypedef -> Result
transVarRefTypedef x = case x of
  Pml.Abs.VarRefTypedefNone -> failure x
  Pml.Abs.VarRefTypedefOne varref -> failure x

transSend :: Pml.Abs.Send -> Result
transSend x = case x of
  Pml.Abs.SendNormal varref sendargs -> failure x
  Pml.Abs.SendSorted varref sendargs -> failure x

transReceive :: Pml.Abs.Receive -> Result
transReceive x = case x of
  Pml.Abs.ReceiveNormal varref recvargs -> failure x
  Pml.Abs.ReceiveRandom varref recvargs -> failure x
  Pml.Abs.ReceivePoll varref recvargs -> failure x
  Pml.Abs.ReceivePollSecond varref recvargs -> failure x

transPoll :: Pml.Abs.Poll -> Result
transPoll x = case x of
  Pml.Abs.PollNoSideEffect varref recvargs -> failure x
  Pml.Abs.PollNoSideEffectSecond varref recvargs -> failure x

transSendArgs :: Pml.Abs.SendArgs -> Result
transSendArgs x = case x of
  Pml.Abs.SendArgs anyexprs -> failure x
  Pml.Abs.SendArgsExpr anyexpr anyexprs -> failure x

transRecvArgs :: Pml.Abs.RecvArgs -> Result
transRecvArgs x = case x of
  Pml.Abs.RecvArgsList recvargs -> failure x
  Pml.Abs.RecvArgsParen recvargs1 recvargs2 -> failure x

transUnaryMinus :: Pml.Abs.UnaryMinus -> Result
transUnaryMinus x = case x of
  Pml.Abs.UnaryMinusNone -> failure x
  Pml.Abs.UnaryMinusOne -> failure x

transRecvArg :: Pml.Abs.RecvArg -> Result
transRecvArg x = case x of
  Pml.Abs.RecvArgRef varref -> failure x
  Pml.Abs.RecvArgEval varref -> failure x
  Pml.Abs.RecvArgConst unaryminus const -> failure x

transAssign :: Pml.Abs.Assign -> Result
transAssign x = case x of
  Pml.Abs.AssignStd varref anyexpr -> failure x
  Pml.Abs.AssignInc varref -> failure x
  Pml.Abs.AssignDec varref -> failure x

transPargs :: Pml.Abs.Pargs -> Result
transPargs x = case x of
  Pml.Abs.PArgsString string -> failure x
  Pml.Abs.PArgsNoString anyexprs -> failure x
  Pml.Abs.PArgsBoth string anyexprs -> failure x

transStmt :: Pml.Abs.Stmt -> Result
transStmt x = case x of
  Pml.Abs.StmtIf options -> failure x
  Pml.Abs.StmtDo options -> failure x
  Pml.Abs.StmtFor range sequence -> failure x
  Pml.Abs.StmtAtomic sequence -> failure x
  Pml.Abs.StmtDAtomic sequence -> failure x
  Pml.Abs.StmtSelect sequence -> failure x
  Pml.Abs.StmtNorm sequence -> failure x
  Pml.Abs.StmtSend send -> failure x
  Pml.Abs.StmtRec receive -> failure x
  Pml.Abs.StmtAssign assign -> failure x
  Pml.Abs.StmtElse -> failure x
  Pml.Abs.StmtBreak -> failure x
  Pml.Abs.StmtGoto pident -> failure x
  Pml.Abs.StmtLabel pident stmt -> failure x
  Pml.Abs.StmtPrint printtype pargs -> failure x
  Pml.Abs.StmtAssert expr -> failure x
  Pml.Abs.StmtCall pident anyexprs -> failure x
  Pml.Abs.StmtExpr expr -> failure x

transRange :: Pml.Abs.Range -> Result
transRange x = case x of
  Pml.Abs.RangeIn pident1 pident2 -> failure x
  Pml.Abs.RangeNoIn pident anyexpr1 anyexpr2 -> failure x

transOptions :: Pml.Abs.Options -> Result
transOptions x = case x of
  Pml.Abs.OptionsOne sequence -> failure x
  Pml.Abs.OptionsCons sequence options -> failure x

transRunPrio :: Pml.Abs.RunPrio -> Result
transRunPrio x = case x of
  Pml.Abs.RunPrioNone -> failure x
  Pml.Abs.RunPrioOne priority -> failure x

transRunArgs :: Pml.Abs.RunArgs -> Result
transRunArgs x = case x of
  Pml.Abs.RunArgsNone -> failure x
  Pml.Abs.RunArgsOne anyexprs -> failure x

transExpr :: Pml.Abs.Expr -> Result
transExpr x = case x of
  Pml.Abs.ExprAny anyexpr -> failure x
  Pml.Abs.ExprParen expr -> failure x
  Pml.Abs.ExprChanPoll chanpoll varref -> failure x
