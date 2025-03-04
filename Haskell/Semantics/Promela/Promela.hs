module Pml.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Control.Monad.Trans.Writer
import GHC.Base
import System.Posix (DL(Null))
import Data.Int (Int32, Int8, Int16)
import qualified Data.ByteString as Data.Bits
import qualified Data.Bits
import Data.Binary (Word32, Word8, Word16, Binary)
import Data.Bits
import GHC.Prelude (fromIntegral)
import GHC.Num
import GHC.Prelude
import qualified Data.Map as Data
import Data.Maybe (fromJust)

data Visible = Visible_hidden | Visible_show
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ChanPoll = ChanPoll_full | ChanPoll1
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Separator = Separator1 | Separator2
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Typename
    = Typename_bit
    | Typename_bool
    | Typename_byte
    | Typename_short
    | Typename_int
    | Typename_mtype
    | Typename_chan
    | TypenamePIdent PIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UnrOp = UnrOp1 | UnrOp2 | UnrOp3
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Const
    = Const_true | Const_false | Const_skip | ConstInteger Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PrintType
    = PrintType_print | PrintType_printf | PrintType_printm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Module
    = Mproc Proctype
    | Minline Inline
    | Minit Init
    | Mnever Never
    | Mtrace Trace
    | Mutype Utype
    | Mmtype Mtype
    | MdeclList DeclList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Proctype
    = Ptype Pactive PIdent PdeclList Ppriority Penabler Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Inline = Iline PIdent [AnyExpr] Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Pactive = PactiveNone | PactiveOne Active
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PdeclList = PdeclListNone | PdeclListOne DeclList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Ppriority = PpriorityNone | PpriorityOne Priority
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Penabler = PenablerNone | PenablerOne Enabler
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Init = Initialise Ipriority Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Ipriority = IpriorityNone | IpriorityOne Priority
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Never = Nvr Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Trace = Trc Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Utype = Utp PIdent DeclList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Mtype = MtpEq Mequals [PIdent] Msep | MtpNoEq [PIdent] Msep
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Msep = MsepNone | MsepOne
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Mequals = Meq
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DeclList
    = DclListOne Decl Separator
    | DclListOneNoSep Decl
    | DclListCons Decl Separator DeclList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl
    = DclOne DeclVisible Typename [Ivar]
    | DclOneUnsigned DeclVisible UnsignedDecl
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DeclVisible = DclVisNone | DclVisOne Visible
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UnsignedDecl = UDcl PIdent Const UDclAssign
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UDclAssign = UDclAssignNone | UdclAssignOne AnyExpr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Active = Active AConst
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AConst = AconstNone | AconstOne Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Priority = Priority Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Enabler = Enabler Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Sequence
    = SeqOne Step
    | SeqOneSep Step Separator
    | SeqNoStep Step Sequence
    | SeqCons Step Separator Sequence
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UStmt = UStmtNone | UStmtOne Stmt
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Step
    = StepMType Mtype
    | StepStmt Stmt UStmt
    | StepDclList DeclList
    | StepXR VarRefList
    | StepXS VarRefList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarRefList
    = VarRefListOne VarRef | VarRefListCons VarRef VarRefList
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AnyExpr
    = AnyExprCond AnyExpr AnyExpr AnyExpr
    | AnyExprlor AnyExpr AnyExpr
    | AnyExprland AnyExpr AnyExpr
    | AnyExprbitor AnyExpr AnyExpr
    | AnyExprbitxor AnyExpr AnyExpr
    | AnyExprbitand AnyExpr AnyExpr
    | AnyExpreq AnyExpr AnyExpr
    | AnyExprneq AnyExpr AnyExpr
    | AnyExprlthan AnyExpr AnyExpr
    | AnyExprgrthan AnyExpr AnyExpr
    | AnyExprle AnyExpr AnyExpr
    | AnyExprge AnyExpr AnyExpr
    | AnyExprleft AnyExpr AnyExpr
    | AnyExprright AnyExpr AnyExpr
    | AnyExprplus AnyExpr AnyExpr
    | AnyExprminus AnyExpr AnyExpr
    | AnyExprtimes AnyExpr AnyExpr
    | AnyExprdiv AnyExpr AnyExpr
    | AnyExprmod AnyExpr AnyExpr
    | AnyExprUnrOp UnrOp AnyExpr
    | AnyExprLen VarRef
    | AnyExprPoll Poll
    | AnyExprVarRef VarRef
    | AnyExprConst Const
    | AnyExprTimeout
    | AnyExprNp
    | AnyExprEnabled AnyExpr
    | AnyExprPCValue AnyExpr
    | AnyExprName PIdent AnyExpr PIdent
    | AnyExprRun PIdent RunArgs RunPrio
    | AnyExprGetPrio Expr
    | AnyExprSetPrio Expr Expr
    | AnyExprStructRef Vars VarRef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Ivar = Ivar PIdent IvarConst IvarAssign
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IvarConst = IvarConstNone | IvarConstOne Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IvarAssign
    = IvarAssignNone
    | IvarAssignAnyExpr AnyExpr
    | IvarAssignChInit ChInit
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ChInit = ChInit Const [Typename]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarRef = VarRef PIdent VarRefAnyExpr VarRefTypedef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarRefAnyExpr = VarRefAnyExprNone | VarRefAnyExprOne AnyExpr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarRefTypedef = VarRefTypedefNone | VarRefTypedefOne VarRef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Send = SendNormal VarRef SendArgs | SendSorted VarRef SendArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Receive
    = ReceiveNormal VarRef RecvArgs
    | ReceiveRandom VarRef RecvArgs
    | ReceivePoll VarRef RecvArgs
    | ReceivePollSecond VarRef RecvArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Poll
    = PollNoSideEffect VarRef RecvArgs
    | PollNoSideEffectSecond VarRef RecvArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SendArgs = SendArgs [AnyExpr] | SendArgsExpr AnyExpr [AnyExpr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RecvArgs
    = RecvArgsList [RecvArg] | RecvArgsParen [RecvArg] RecvArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UnaryMinus = UnaryMinusNone | UnaryMinusOne
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RecvArg
    = RecvArgRef VarRef
    | RecvArgEval VarRef
    | RecvArgConst UnaryMinus Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Assign
    = AssignStd VarRef AnyExpr | AssignInc VarRef | AssignDec VarRef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Pargs
    = PArgsString String
    | PArgsNoString [AnyExpr]
    | PArgsBoth String [AnyExpr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = StmtIf Options
    | StmtDo Options
    | StmtFor Range Sequence
    | StmtAtomic Sequence
    | StmtDAtomic Sequence
    | StmtSelect Sequence
    | StmtNorm Sequence
    | StmtSend Send
    | StmtRec Receive
    | StmtAssign Assign
    | StmtElse
    | StmtBreak
    | StmtGoto PIdent
    | StmtLabel PIdent Stmt
    | StmtPrint PrintType Pargs
    | StmtAssert Expr
    | StmtCall PIdent [AnyExpr]
    | StmtExpr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Range
    = RangeIn PIdent PIdent | RangeNoIn PIdent AnyExpr AnyExpr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Options = OptionsOne Sequence | OptionsCons Sequence Options
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RunPrio = RunPrioNone | RunPrioOne Priority
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RunArgs = RunArgsNone | RunArgsOne [AnyExpr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = ExprAny AnyExpr | ExprParen Expr | ExprChanPoll ChanPoll VarRef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PIdent = PIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Vars = 
    Bl Bool 
    | By Int8
    | UBy Word8
    | Sh Int16
    | USh Word16
    | I Int32
    | UI Word32
    | Bs Int32
    | S (Map.Map PIdent Vars)
    | BlL [Bool]
    | ByL [Int8]
    | UByL [Word8] 
    | ShL [Int16]
    | UShL [Word16]
    | IL [Int32] 
    | BsL [Int32] 
    | UIL [Word32]
    | SL [Map.Map PIdent Vars]
    | M Mtype
    | ML [Mtype]
    | Null
    deriving (C.Eq, C.Show, C.Read, C.Ord)

data TState = Zombie | Dormant | Ready | Blocked | TimeWait | Otherwait

type ProgState a = StateT (ProgMem, ControlMem) (WriterT [String] IO) a -- Programme State
type ControlMem = (Integer, Integer, Proctype, Inline, Stmt, TStates) -- Control Memory is defined as a 6-tuple of current thread, last executed thread, proctypes, inlines, labels and thread states
type TStates = (Map.Map Integer TState) -- Thread states are defined as a mapping between threadId and thread state
type ProgMem = (Map.Map Integer Mem, Mem) -- Programme memory is a tuple of local memory (A mapping between threadId and Memory) and Global Memory (Mem)
type Mem = Map.Map PIdent (Vars, Typename) -- Memory is modelled as a mapping from identifiers to a (variable, type) tuple

evalExpr :: AnyExpr -> ProgState Vars
evalExpr (AnyExprCond c p q) = do
    c' <- evalExpr c
    case c' of 
        (Bl a)
            | a -> evalExpr p
            | otherwise -> evalExpr q
        _ -> error "Conditional expression does not evaluate to boolean"
evalExpr (AnyExprland e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a && b
evalExpr (AnyExprlor e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a || b
evalExpr (AnyExprbitand e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum ((.&.) :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprbitor e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum ((.|.) :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprbitxor e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum (xor :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExpreq e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a == b
        _ -> return $ evalOpBool ((==) :: Int -> Int -> Bool) e1' e2'
evalExpr (AnyExprneq e1 e2) = do
    (Bl a) <- evalExpr (AnyExpreq e1 e2)
    return $ Bl . not $ a
evalExpr (AnyExprlthan e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpBool ((<) :: Int -> Int -> Bool) e1' e2'
evalExpr (AnyExprgrthan e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpBool ((>) :: Int -> Int -> Bool) e1' e2'
evalExpr (AnyExprge e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpBool ((>=) :: Int -> Int -> Bool) e1' e2'
evalExpr (AnyExprle e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpBool ((<=) :: Int -> Int -> Bool) e1' e2'
evalExpr (AnyExprleft e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum (shift :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprright e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum (shift :: Int -> Int -> Int) e1' (evalUnrOp UnrOp2 e2')
evalExpr (AnyExprplus e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum ((+) :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprminus e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum ((-) :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprtimes e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum ((*) :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprdiv e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum (div :: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprmod e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    return $ evalOpNum (mod:: Int -> Int -> Int) e1' e2'
evalExpr (AnyExprUnrOp op e) = do
    e' <- evalExpr e
    return $ evalUnrOp op e'
evalExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone VarRefTypedefNone)) = do -- Simplest case, struct reference is just a reference to a variable
    ((local, global), cm@(tid, _, _, _, _, _)) <- get
    case Data.lookup id global of -- Check globals
         (Just (v, _)) -> return v
         Nothing -> 
            case Data.lookup id (fromJust $ Data.lookup tid local) of -- then check locals
                (Just (v, _)) -> return v
                Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then cry and give up
evalExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone )) = do -- Now it's a reference to an array
    ((local, global), cm@(tid, _, _, _, _, _)) <- get
    e' <- evalExpr e -- so evaluate the array index
    let idx :: Int = case e' of -- cast to an int
            (UI a) -> fromIntegral a
            (I a) -> fromIntegral a
            (USh a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (UBy a) -> fromIntegral a
            (By a) -> fromIntegral a

    case Data.lookup id global of -- look for the array globally
         (Just (v, _)) -> listIndex v idx
         Nothing -> 
            case Data.lookup id (fromJust $ Data.lookup tid local) of -- look for it locally
                (Just (v, _)) -> listIndex v idx
                Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then give up again

evalExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr))) = do -- We are now looking for an element of a struct
    struct <- evalExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone VarRefTypedefNone)) -- so first find the struct
    evalExpr (AnyExprStructRef struct vr) -- Then find the element

evalExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr))) = do -- We are looking for an element in a struct, in an array
    struct <- evalExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone)) -- so first index the array for the struct
    evalExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr))) -- Then find the element in the struct
    
evalExpr (AnyExprStructRef struct (VarRef id VarRefAnyExprNone VarRefTypedefNone)) = -- Logic for finding elements in a struct
    case (struct, id) of -- lookup the element in the struct
        (S m, id) ->  return . fromJust $ Data.lookup id m -- return it
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalExpr (AnyExprStructRef struct (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr))) =
    case (struct, id) of
        (S m, id) ->  evalExpr (AnyExprStructRef (fromJust $ Data.lookup id m) vr) -- look up the element in the struct and recursively search using variable reference
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalExpr (AnyExprStructRef struct (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr))) = do -- we are looking for an element of an element of a struct that is itself an element of a list 
    e' <- evalExpr e -- evaluate index
    let idx :: Int = case e' of
            (UI a) -> fromIntegral a
            (I a) -> fromIntegral a
            (USh a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (UBy a) -> fromIntegral a
            (By a) -> fromIntegral a
    case (struct, id) of -- look up the 
        (S m, id) -> do
             lv <- listIndex (fromJust $ Data.lookup id m) idx
             evalExpr (AnyExprStructRef lv vr)
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalExpr (AnyExprStructRef struct (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone)) = do -- looking for an element of a struct that is itself an element of a list
    e' <- evalExpr e
    let idx :: Int = case e' of
            (UI a) -> fromIntegral a
            (I a) -> fromIntegral a
            (USh a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (UBy a) -> fromIntegral a
            (By a) -> fromIntegral a
    case (struct, id) of
        (S m, id) -> listIndex (fromJust $ Data.lookup id m) idx
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

listIndex :: Vars -> Int -> ProgState Vars
listIndex vars idx = case vars of
    (BlL v) -> return . Bl $ v !! idx
    (ByL v) -> return . By $ v !! idx
    (UByL v) -> return . UBy $ v !! idx
    (ShL v) -> return . Sh $ v !! idx
    (UShL v) -> return . USh $ v !! idx
    (IL v) -> return . I $ v !! idx
    (UIL v) -> return . UI $ v !! idx

evalUnrOp :: UnrOp -> Vars -> Vars
evalUnrOp UnrOp2 (Bl False) = Bl True
evalUnrOp UnrOp2 (Bl True) = Bl False
evalUnrOp UnrOp1 (By a) = By $ complement a
evalUnrOp UnrOp2 (By a) = By $ - a
evalUnrOp UnrOp3 (By 0) = By 1 
evalUnrOp UnrOp3 (By a) = By 0 
evalUnrOp UnrOp1 (UBy a) = UBy $ complement a
evalUnrOp UnrOp2 (UBy a) = UBy $ - a
evalUnrOp UnrOp3 (UBy 0) = UBy 1 
evalUnrOp UnrOp3 (UBy a) = UBy 0 
evalUnrOp UnrOp1 (Sh a) = Sh $ complement a
evalUnrOp UnrOp2 (Sh a) = Sh $ - a
evalUnrOp UnrOp3 (Sh 0) = Sh 1 
evalUnrOp UnrOp3 (Sh a) = Sh 0 
evalUnrOp UnrOp1 (USh a) = USh $ complement a
evalUnrOp UnrOp2 (USh a) = USh $ - a
evalUnrOp UnrOp3 (USh 0) = USh 1 
evalUnrOp UnrOp3 (USh a) = USh 0 
evalUnrOp UnrOp1 (I a) = I $ complement a
evalUnrOp UnrOp2 (I a) = I $ - a
evalUnrOp UnrOp3 (I 0) = I 1 
evalUnrOp UnrOp3 (I a) = I 0 
evalUnrOp UnrOp1 (UI a) = UI $ complement a
evalUnrOp UnrOp2 (UI a) = UI $ - a
evalUnrOp UnrOp3 (UI 0) = UI 1 
evalUnrOp UnrOp3 (UI a) = UI 0 
evalUnrOp UnrOp1 (Bs a) = Bs $ complement a
evalUnrOp UnrOp2 (Bs a) = Bs $ - a
evalUnrOp UnrOp3 (Bs 0) = Bs 1 
evalUnrOp UnrOp3 (Bs a) = Bs 0 

applyOp :: (Bits a1, Integral a1, Bits a2, Integral a2, Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> t3) -> a1 -> a2 -> t3
applyOp op a b = fromIntegral a `op` fromIntegral b 

evalOpNum :: (Bits a, Integral a, Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> a) -> Vars -> Vars -> Vars
evalOpNum op (By a) (By b) = By . fromIntegral $ applyOp op a b
evalOpNum op (By a) (UBy b) = By . fromIntegral $ applyOp op a b
evalOpNum op (By a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (By a) (USh b) = USh . fromIntegral $ applyOp op a b
evalOpNum op (By a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (By a) (UI b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (By b) = UBy . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (UBy b) = UBy . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (USh b) = USh . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (UBy a) (UI b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (By b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (UBy b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (USh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (UI b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (By b) = USh . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (UBy b) = USh . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (USh b) = USh . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (USh a) (UI b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (I a) (By b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (UBy b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (Sh b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (USh b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (UI b) = I . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (By b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (UBy b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (Sh b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (USh b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (UI a) (UI b) = UI . fromIntegral $ applyOp op a b
evalOpNum op (Bs a) (Bs b) = Bs . fromIntegral $ applyOp op a b

evalOpBool :: (Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> Bool) -> Vars -> Vars -> Vars
evalOpBool op (By a) (By b) = Bl $ applyOp op a b
evalOpBool op (By a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (By a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (By a) (USh b) = Bl $ applyOp op a b
evalOpBool op (By a) (I b) = Bl $ applyOp op a b
evalOpBool op (By a) (UI b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (By b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (USh b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (I b) = Bl $ applyOp op a b
evalOpBool op (UBy a) (UI b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (By b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (USh b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (I b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (UI b) = Bl $ applyOp op a b
evalOpBool op (USh a) (By b) = Bl $ applyOp op a b
evalOpBool op (USh a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (USh a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (USh a) (USh b) = Bl $ applyOp op a b
evalOpBool op (USh a) (I b) = Bl $ applyOp op a b
evalOpBool op (USh a) (UI b) = Bl $ applyOp op a b
evalOpBool op (I a) (By b) = Bl $ applyOp op a b
evalOpBool op (I a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (I a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (I a) (USh b) = Bl $ applyOp op a b
evalOpBool op (I a) (I b) = Bl $ applyOp op a b
evalOpBool op (I a) (UI b) = Bl $ applyOp op a b
evalOpBool op (UI a) (By b) = Bl $ applyOp op a b
evalOpBool op (UI a) (UBy b) = Bl $ applyOp op a b
evalOpBool op (UI a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (UI a) (USh b) = Bl $ applyOp op a b
evalOpBool op (UI a) (I b) = Bl $ applyOp op a b
evalOpBool op (UI a) (UI b) = Bl $ applyOp op a b
evalOpBool op (Bs a) (Bs b) = Bl $ applyOp op a b