{-# LANGUAGE LambdaCase #-}
module Pml.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Control.Monad.Trans.Writer
import GHC.Base
import Data.Int (Int32, Int8, Int16)
import qualified Data.Bits
import Data.Binary (Word32, Word8, Word16, Binary)
import GHC.Prelude
import qualified Data.Map as Data
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (elemIndex, find, partition, delete)
import System.Random (randomRIO)
import Control.Monad (when)
import qualified Data.Text
import Control.Monad.Trans.Class (MonadTrans(lift))

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
    | Typename_pid
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
    | SeqNoSep Step Sequence
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
    | Sh Int16
    | I Int32
    | UI Word32 Int
    | Bs Int32
    | S (Map.Map PIdent Vars)
    | BlL [Bool]
    | ByL [Int8]
    | ShL [Int16]
    | IL [Int32] 
    | BsL [Int32] 
    | UIL [Word32] Int
    | SL [Map.Map PIdent Vars]
    deriving (C.Eq, C.Show, C.Read, C.Ord)

data TState = Zombie | Dormant | Ready | Blocked | TimeWait | Otherwait
  deriving (C.Eq, C.Show)

type ProgState a = StateT (ProgMem, ControlMem) (WriterT [String] IO) a -- Programme State
type ControlMem = (Integer, [Proctype], [Inline], Labels, TStates, Structs) -- Control Memory is defined as a 6-tuple of last executed thread, proctypes, inlines, labels, thread states and structs
type Structs = Map.Map PIdent Vars -- Struct types are defined as a mapping between an identifier and a default initialised struct 
type Labels = Map.Map PIdent Sequence -- Labels are a mapping between an identifier and a programme sequence
type TStates = (Map.Map Integer (TState, Maybe Sequence, [Sequence])) -- Thread states are defined as a mapping between threadId and the tuple of Tstate and Sequence, representing the sequence of statements yet to be taken
type ProgMem = (Map.Map Integer Mem, Mem) -- Programme memory is a tuple of local memory (A mapping between threadId and Memory) and Global Memory (Mem)
type Mem = Map.Map PIdent (Vars, Typename) -- Memory is modelled as a mapping from identifiers to a (variable, type) tuple

runProgState :: [Pml.Abs.Module] -> IO ()
runProgState prog = do
  putStr "\n\n=====Programme Output=====\n\n"
  st <- runWriterT $ (runStateT $ runProg prog) 
    ((Map.fromList [(0, Map.empty)], Map.fromList [(PIdent "_nr_pr", (I 1, Typename_int))]), 
    (0::Integer, [], [], Map.empty, Map.fromList [(0, (Ready, Nothing, []))], Map.empty))
  case st of 
    (((), ((threads, globals), (tid, p, i, l, tstates, structs))), log) -> putStr $
      "Threads: " ++ show threads ++ "\n" ++
      "Global Vars: " ++ show globals ++ "\n" ++
      "Last TID: " ++ show tid ++ "\n" ++
      "Tstates: " ++ show tstates ++ "\n" ++
      "Log: " ++ show log ++ "\n"

runProg :: [Pml.Abs.Module] -> ProgState ()
runProg ms = do
  mapM_ evalModule ms
  execProgramme

evalDcl :: Decl -> ProgState [(Vars, Typename, PIdent)]
evalDcl (DclOne _ _ []) = return []
evalDcl (DclOne _ typename ((Ivar ident const assign):ivs)) = do
          ((_, _), (tid, _, _, _, _, structs)) <- get
          case (const, assign) of 
            (IvarConstNone, IvarAssignNone) -> do
              let val = case typename of 
                    Typename_bit -> Bs 0
                    Typename_bool -> Bl False
                    Typename_byte -> By 0
                    Typename_short -> Sh 0
                    Typename_int -> I 0
                    Typename_pid -> I 0
                    (TypenamePIdent id) -> fromJust $ Data.lookup id structs
              dcls <- evalDcl (DclOne DclVisNone typename ivs)
              return $ (val, typename, ident):dcls
            (IvarConstOne const, IvarAssignNone) -> do
              let val = case const of
                    (ConstInteger i) -> case typename of
                        Typename_bool -> BlL $ replicate (fromIntegral i) False
                        Typename_bit -> BsL $ replicate (fromIntegral i) 0 
                        Typename_byte -> ByL $ replicate (fromIntegral i) 0
                        Typename_short -> ShL $ replicate (fromIntegral i) 0
                        Typename_int -> IL $ replicate (fromIntegral i) 0
                        Typename_pid -> IL $ replicate (fromIntegral i) 0
                        (TypenamePIdent id) -> case fromJust $ Data.lookup id structs of 
                          (S str) -> SL $ replicate (fromIntegral i) str
                          _ -> error "Struct array was not a struct"
              dcls <- evalDcl (DclOne DclVisNone typename ivs)
              return $ (val, typename, ident):dcls
            (IvarConstNone, IvarAssignAnyExpr e) -> do 
              val <- evalAnyExpr e
              dcls <- evalDcl (DclOne DclVisNone typename ivs)
              return $ (val, typename, ident):dcls
            (IvarConstOne const, IvarAssignAnyExpr e) -> do 
              rval <- evalAnyExpr e
              let elem = case rval of 
                    (I v) -> v
                    (UI v b) -> fromIntegral v
                    (Sh v) -> fromIntegral v
                    (By v) -> fromIntegral v
              let val = case const of
                    (ConstInteger i) -> case typename of
                        Typename_bit -> BsL $ replicate (fromIntegral i) elem
                        Typename_byte -> ByL $ replicate (fromIntegral i) (fromIntegral elem)
                        Typename_short -> ShL $ replicate (fromIntegral i) (fromIntegral elem)
                        Typename_int -> IL $ replicate (fromIntegral i) elem
                        Typename_pid -> IL $ replicate (fromIntegral i) elem
              dcls <- evalDcl (DclOne DclVisNone typename ivs)
              return $ (val, typename, ident):dcls

evalDcl (DclOneUnsigned _ (UDcl ident const assign)) = do
  ((_, _), (tid, _, _, _, _, _)) <- get
  let bits = case const of
        (ConstInteger c) -> c
        _ -> error "unsigned constant must be an integer" 
  case assign of 
    (UdclAssignOne e) -> do
      e' <- evalAnyExpr e
      let v = case e' of 
            (By a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (Bs a) -> fromIntegral a
            (I a) -> fromIntegral a
            (UI a _) -> fromIntegral a
      return [(UI (v .&. (2^bits)-1) (fromIntegral bits), TypenamePIdent . PIdent $ "unsigned", ident)]
    UDclAssignNone -> return [(UI 0 (fromIntegral bits), TypenamePIdent . PIdent $ "unsigned", ident)]
            
evalModule :: Pml.Abs.Module -> ProgState ()
evalModule m@(Mproc p) = do 
  (pm, (tid, ps, is, labels, tstate, structs)) <- get
  let labels' = searchLabelsModule m `Map.union` labels
  put (pm, (tid, p:ps, is, labels', tstate, structs))
evalModule (MdeclList dl) = do
  case dl of 
    (DclListOneNoSep dcl) -> do
      ((locals, globals), c) <- get
      dcls <- evalDcl dcl
      let globals' = setMem dcls globals
      put ((locals, globals'), c)
    (DclListOne dcl _) -> evalModule (MdeclList $ DclListOneNoSep dcl)
    (DclListCons dcl _ dcls) -> do
      evalModule (MdeclList $ DclListOneNoSep dcl)
      evalModule (MdeclList dcls)
evalModule (Mmtype m) = do
  (p@(threads, globals), c@(_, _, _, _, _, tid)) <- get
  let ms = case m of 
        (MtpEq _ ms _) -> ms
        (MtpNoEq ms _) -> ms
  let globals' = Data.union (Data.fromList $ zip (reverse ms) (zip (map I [1..]) (repeat . TypenamePIdent . PIdent $ "mtype"))) globals
  put ((threads, globals'), c)
evalModule (Mutype (Utp ident dl)) = do
  struct <- createStruct dl Map.empty
  (pm, (tid, ps, is, labels, tstate, structs)) <- get
  let structs' = Data.insert ident (S struct) structs
  put (pm, (tid, ps, is, labels, tstate, structs'))
evalModule (Minline i) = do 
  (pm, (tid, ps, is, labels, tstate, structs)) <- get
  put (pm, (tid, ps, i:is, labels, tstate, structs))
evalModule m@(Minit (Initialise _ sequence)) = do
  ((threads, globals), (tid, ps, is, labels, tstate, structs)) <- get
  let labels' = searchLabelsModule m `Map.union` labels
  let threads' = Data.insert 0 (Map.fromList [(PIdent "_pid" , (I 0, Typename_pid))]) threads
  let tstate' = Data.insert 0 (Ready, Nothing, [sequence]) tstate
  put ((threads', globals), (tid, ps, is, labels', tstate', structs))

appendSeq :: Sequence -> Sequence -> Sequence
appendSeq (SeqOne step) seq = SeqNoSep step seq
appendSeq (SeqOneSep step _) seq = SeqNoSep step seq
appendSeq (SeqNoSep step rm) seq = SeqNoSep step (appendSeq rm seq)
appendSeq (SeqCons step _ rm) seq = SeqNoSep step (appendSeq rm seq)

searchLabelsModule :: Pml.Abs.Module -> Labels
searchLabelsModule (Minit (Initialise _ seq)) = searchLabelsSeq seq
searchLabelsModule (Mproc (Ptype _ _ _ _ _ seq)) = searchLabelsSeq seq

searchLabelsSeq :: Sequence -> Labels
searchLabelsSeq (SeqOne (StepStmt s _)) = 
  case s of 
    (StmtLabel label stmt) -> Map.fromList [(label, SeqOne . flip StepStmt UStmtNone $ stmt)]
    _ -> searchLabelsStmt s
searchLabelsSeq (SeqOneSep (StepStmt s _) _) = 
  case s of 
    (StmtLabel label stmt) -> Map.fromList [(label, SeqOne . flip StepStmt UStmtNone $ stmt)]
    _ -> searchLabelsStmt s
searchLabelsSeq (SeqNoSep step seq) = 
  case step of 
    (StepStmt (StmtLabel ident stmt) _) -> 
      searchLabelsStmt stmt `Map.union` 
      Map.fromList [(ident, SeqNoSep (StepStmt stmt UStmtNone) seq)] `Map.union`
      searchLabelsSeq seq
    (StepStmt stmt _) -> searchLabelsStmt stmt `Map.union` searchLabelsSeq seq
    _ -> searchLabelsSeq seq
searchLabelsSeq (SeqCons step _ seq) = 
  case step of 
    (StepStmt (StmtLabel ident stmt) _) -> 
      searchLabelsStmt stmt `Map.union` 
      Map.fromList [(ident, SeqNoSep (StepStmt stmt UStmtNone) seq)] `Map.union`
      searchLabelsSeq seq
    _ -> searchLabelsSeq seq

searchLabelsStmt :: Stmt -> Labels
searchLabelsStmt (StmtAtomic seq) = 
  fmap (SeqOne . flip StepStmt UStmtNone . StmtAtomic) (searchLabelsSeq seq)
searchLabelsStmt (StmtDo opts) = 
  let loop = SeqOne (StepStmt (StmtDo opts) UStmtNone) in
  fmap (`appendSeq` loop) (foldl Map.union Map.empty (fmap searchLabelsSeq (optsToSeq opts)))
searchLabelsStmt (StmtIf opts) = 
  foldl Map.union Map.empty (fmap searchLabelsSeq (optsToSeq opts))
searchLabelsStmt _ = Map.empty

setMem :: [(Vars, Typename, PIdent)] -> Data.Map PIdent (Vars, Typename) -> Data.Map PIdent (Vars, Typename)
setMem [] = id
setMem ((val, typename, ident):xs) = Map.insert ident (val, typename) <$> setMem xs

setMemStruct :: [(Vars, Typename, PIdent)] -> Data.Map PIdent Vars -> Data.Map PIdent Vars
setMemStruct [] = id
setMemStruct ((val, _, ident):xs) = Map.insert ident val <$> setMemStruct xs

createStruct :: DeclList -> Map.Map PIdent Vars -> ProgState (Map.Map PIdent Vars)
createStruct dl map = 
  case dl of 
    (DclListOneNoSep dcl) -> do
      dcls <- evalDcl dcl
      return $ setMemStruct dcls map
    (DclListOne dcl _) -> createStruct (DclListOneNoSep dcl) map
    (DclListCons dcl _ dcls) -> do
      map' <- createStruct (DclListOneNoSep dcl) map
      createStruct dcls map'

execProgramme :: ProgState ()
execProgramme = do 
  (_, (_, _, _, _, tstates, _)) <- get
  let tids = Data.keys tstates
  -- This is awful but I can explain
  -- Consider the scenario in which all threads are waiting for the previous thread to be unblocked for themselves
  -- to be unblocked (Dependencies, yay!). So, thread 4 will be unblocked when thread 5 is unblocked and so on
  -- We iterate over our threadIds but we can only unblock 5 as that isn't waiting on anything.
  -- It would now erroneously appear that thread 4 is still blocked.
  -- To fix this we do an equal amount of passes as there are threads to allow changes to propagate back
  -- Quick optimisation I do not have time for: only back propagate if change is discovered and only relative to
  -- change.
  mapM_ (const (mapM_ updateBlocked tids)) tids
  (pm, c@(tid, p, i, l, tstates, s)) <- get
  let execTids = map fst (filter (\(_, (st, _, _)) -> st == Ready) (Map.toList tstates))
  case execTids of
    [] -> liftIO $ putStr "\n\n=====FINAL STATE=====\n\nNo 'Ready' threads available, programme terminated.\n\n"
    _ -> do
      tidIdx <- liftIO $ randomRIO (0, length execTids -1)
      let tid' = execTids !! tidIdx
      lift . tell $ ["Thread " ++ show tid' ++ " scheduled, could also have scheduled: " ++ show (Data.List.delete tid' execTids)]
      put (pm, (tid', p, i, l, tstates, s))
      execThread
      execProgramme

updateBlocked :: Integer -> ProgState ()
updateBlocked tid = do
  (pm@(threads, globals), c@(tid', p, i, l, tstates, s)) <- get
  let (state, seq, rm) = fromJust $ Data.lookup tid tstates
  put (pm, (tid, p, i, l, tstates, s))
  case seq of 
    Nothing -> 
      case rm of
        (x:xs) -> do 
          ex <- executableSeq x
          let tstate = if ex then Ready else Blocked
          let tstates' = Data.insert tid (tstate, seq, x:xs) tstates
          put (pm, (tid', p, i, l, tstates', s))
        [] -> do 
          when (state /= Zombie) 
            (do 
              let tstates' = Data.insert tid (Zombie, Nothing, []) tstates
              let (I a, _) = fromJust $ Data.lookup (PIdent "_nr_pr") globals
              let globals' = Data.insert (PIdent "_nr_pr") (I (a-1), Typename_int) globals
              put ((threads, globals'), (tid', p, i, l, tstates', s)))
    (Just x) -> do
      ex <- executableSeq x
      let ready = if ex then Ready else Blocked
      let tstates' = Data.insert tid (ready, seq, rm) tstates
      put (pm, (tid', p, i, l, tstates', s))

execThread :: ProgState ()
execThread = do
  (pm@(threads, globals), c@(tid, p, i, l, tstates, s)) <- get
  let (st, seq, rm) = fromJust $ Data.lookup tid tstates
  case seq of 
    Nothing -> case rm of 
      (x:xs) -> do 
        let tstates' = Data.insert tid (st, Just x, xs) tstates
        put (pm, (tid, p, i, l, tstates', s))
        evalSequence x
      [] -> do
        let tstates' = Data.insert tid (Zombie, Nothing, []) tstates
        put (pm, (tid, p, i, l, tstates', s))
    (Just seq) -> evalSequence seq

execThreadAtm :: ProgState ()
execThreadAtm = do
  (pm@(threads, globals), c@(tid, p, i, l, tstates, s)) <- get
  let (st, seq, rm) = fromJust $ Data.lookup tid tstates
  case seq of 
    Nothing -> case rm of 
      (x:xs) -> do 
        let tstates' = Data.insert tid (st, Just x, xs) tstates
        put (pm, (tid, p, i, l, tstates', s))
        evalSequenceAtm x
      [] -> do
        let tstates' = Data.insert tid (Zombie, Nothing, []) tstates
        put (pm, (tid, p, i, l, tstates', s))
    (Just seq) -> evalSequenceAtm seq

evalSequenceAtm :: Sequence -> ProgState ()
evalSequenceAtm (SeqOne step) = evalStep step
evalSequenceAtm (SeqOneSep step _) = evalStep step
evalSequenceAtm (SeqNoSep step seq) = do
    (pm, c@(tid, p, i, l, tstates, s)) <- get
    let (st, curr, rm) = fromJust $ Data.lookup tid tstates
    let tstates' = Data.insert tid (Ready, Just $ SeqOne step, seq:rm) tstates
    put (pm, (tid, p, i, l, tstates', s))
    evalStep step
    execThreadAtm
evalSequenceAtm (SeqCons step _ seq) = do
    (pm, c@(tid, p, i, l, tstates, s)) <- get
    let (st, curr, rm) = fromJust $ Data.lookup tid tstates
    let tstates' = Data.insert tid (st, Just $ SeqOne step, seq:rm) tstates
    put (pm, (tid, p, i, l, tstates', s))
    evalStep step
    execThreadAtm

evalSequence :: Sequence -> ProgState ()
evalSequence (SeqOne step) = evalStep step
evalSequence (SeqOneSep step _) = evalStep step
evalSequence (SeqNoSep step seq) = do
    (pm, c@(tid, p, i, l, tstates, s)) <- get
    let (st, curr, rm) = fromJust $ Data.lookup tid tstates
    let tstates' = Data.insert tid (Ready, Just $ SeqOne step, seq:rm) tstates
    put (pm, (tid, p, i, l, tstates', s))
    evalStep step
evalSequence (SeqCons step _ seq) = do
    (pm, c@(tid, p, i, l, tstates, s)) <- get
    let (st, curr, rm) = fromJust $ Data.lookup tid tstates
    let tstates' = Data.insert tid (st, Just $ SeqOne step, seq:rm) tstates
    put (pm, (tid, p, i, l, tstates', s))
    evalStep step

evalStep :: Step -> ProgState ()
evalStep (StepMType m) = do
    (p@(threads, g), (tid, pr, i, l, tstates, s)) <- get
    let locals = fromJust $ Data.lookup tid threads
    case m of 
        (MtpEq _ ms _) -> do
            let update = Data.union (Data.fromList $ zip (reverse ms) (zip (map I [1..]) (repeat . TypenamePIdent . PIdent $ "mtype"))) locals
            let newthreads = Data.insert tid update threads
            let (st, curr, rm) = fromJust $ Data.lookup tid tstates
            let tstates' = Data.insert tid (Ready, Nothing, rm) tstates
            put ((newthreads, g), (tid, pr, i, l, tstates', s))
        (MtpNoEq ms _) -> do
            let update = Data.union (Data.fromList $ zip (reverse ms) (zip (map I [1..]) (repeat . TypenamePIdent . PIdent $ "mtype"))) locals
            let newthreads = Data.insert tid update threads
            let (st, curr, rm) = fromJust $ Data.lookup tid tstates
            let tstates' = Data.insert tid (Ready, Nothing, rm) tstates
            put ((newthreads, g), (tid, pr, i, l, tstates', s))
evalStep (StepStmt s su) = do
    evalStmt s
    case su of 
        (UStmtOne s') -> evalStmt s'
        UStmtNone -> return ()

evalStep (StepDclList dcls) = do
    case dcls of 
        (DclListOneNoSep dcl) -> do
          dcls <- evalDcl dcl
          ((threads, g), c@(tid, p, i, l, tstates, s)) <- get
          let locals = fromJust $ Data.lookup tid threads
          let threads' = Data.insert tid (setMem dcls locals) threads
          let (st, curr, rm) = fromJust $ Data.lookup tid tstates
          let tstates' = Data.insert tid (st, Nothing, rm) tstates
          put ((threads', g), (tid, p, i, l, tstates', s))
        (DclListOne dcl _) -> evalStep (StepDclList (DclListOneNoSep dcl))
        (DclListCons dcl _ dcls') -> do
            evalStep (StepDclList (DclListOneNoSep dcl))
            (pm, c@(tid, p, i, l, tstates, s)) <- get
            let (st, curr, rm) = fromJust $ Data.lookup tid tstates
            let tstates' = Data.insert tid (st, Nothing, SeqOne (StepDclList dcls'):rm) tstates
            put (pm, (tid, p, i, l, tstates', s))

optsToSeq :: Options -> [Sequence]
optsToSeq (OptionsOne s) = [s]
optsToSeq (OptionsCons s o) = s:optsToSeq o

filterSeq :: [Sequence] -> ProgState [Sequence]
filterSeq [] = return []
filterSeq (x:xs) = do
  executable <- executableSeq x
  if executable
    then do 
      seqs <- filterSeq xs
      return (x:seqs)
    else filterSeq xs

partitionSeqs :: Sequence -> Bool
partitionSeqs s = 
  case s of 
    SeqCons (StepStmt StmtElse _) _ _ -> False
    SeqOne (StepStmt StmtElse _) -> False
    SeqOneSep (StepStmt StmtElse _) _ -> False
    SeqNoSep (StepStmt StmtElse _) _ -> False
    _ -> True

iterateSeq :: ProgState ()
iterateSeq = do
  (pm, c@(tid, p, i, l, tstates, s)) <- get
  let (st, curr, rm) = fromJust $ Data.lookup tid tstates
  let tstates' = Data.insert tid (Ready, Nothing, rm) tstates
  put (pm, (tid, p, i, l, tstates', s))

evalStmt :: Stmt -> ProgState ()
evalStmt (StmtAtomic seq) = do 
  evalSequenceAtm seq
evalStmt (StmtDAtomic seq) = evalSequenceAtm seq
evalStmt (StmtDo opts) = do
  seqs <- filterSeq $ optsToSeq opts
  let pseqs@(validSeqs, _) = partition partitionSeqs seqs
  seqIdx <- liftIO $ randomRIO (0, length validSeqs - 1)
  let seq = case pseqs of
        ([], elseSeqs) -> head elseSeqs
        (validSeqs, _) -> do 
            seqs !! seqIdx
  (pm, c@(tid, p, i, l, tstates, s)) <- get
  let loop = SeqOne $ StepStmt (StmtDo opts) UStmtNone
  let (st, curr, rm) = fromJust $ Data.lookup tid tstates
  let tstates' = Data.insert tid (Ready, Just seq, loop:rm) tstates
  put (pm, (tid, p, i, l, tstates', s))
evalStmt (StmtIf opts) = do
  seqs <- filterSeq $ optsToSeq opts
  seqIdx <- liftIO $ randomRIO (0, length seqs - 1)
  let seq = seqs !! seqIdx
  (pm, c@(tid, p, i, l, tstates, s)) <- get
  let (st, curr, rm) = fromJust $ Data.lookup tid tstates
  let tstates' = Data.insert tid (Ready, Just seq, rm) tstates
  put (pm, (tid, p, i, l, tstates', s))
evalStmt (StmtAssign a) = do
  ((threads, g), c@(tid, _, _, _, _, _)) <- get
  case a of 
    (AssignStd var expr) -> do
      e <- evalAnyExpr expr
      updateVar var e
    (AssignInc var) -> do
      val <- evalAnyExpr (AnyExprplus (AnyExprVarRef var) (AnyExprConst . ConstInteger $ 1))
      updateVar var val
    (AssignDec var) -> do
      val <- evalAnyExpr (AnyExprminus (AnyExprVarRef var) (AnyExprConst . ConstInteger $ 1))
      updateVar var val
  iterateSeq
evalStmt (StmtPrint _ args) = do 
  str <- evalPrint args
  liftIO . putStr $ str
  iterateSeq
evalStmt (StmtAssert e) = do 
  rv <- executableStmt (StmtExpr e)
  if rv then return () else error $ "assertion violated" ++ show e
  iterateSeq
evalStmt (StmtExpr ( ExprAny (AnyExprRun ident args prio))) = do
  evalAnyExpr (AnyExprRun ident args prio)
  iterateSeq
evalStmt se@(StmtExpr e) = do 
  executable <- executableStmt se
  (pm, (tid, p, i, l, tstates, s)) <- get
  let (st, seq, rm) = fromJust $ Data.lookup tid tstates
  if executable 
    then put (pm, (tid, p, i, l, Data.insert tid (Ready, Nothing, rm) tstates, s)) else
    put (pm, (tid, p, i, l, Data.insert tid (Blocked, Nothing, rm) tstates, s))
evalStmt StmtBreak = do
  (pm, (tid, p, i, l, tstates, s)) <- get
  let (st, seq, rm) = fromJust $ Data.lookup tid tstates
  let xs = dropWhile (\case (SeqOne ( StepStmt ( StmtDo _) _)) -> True; _ -> False;) rm
  let tstates' = Data.insert tid (Ready, Nothing, xs) tstates
  put (pm, (tid, p, i, l, tstates', s))
evalStmt StmtElse = iterateSeq
evalStmt (StmtGoto label) = do
  (pm, (tid, p, i, l, tstates, s)) <- get
  let (st, curr, rm) = fromJust $ Data.lookup tid tstates
  let seq = fromJust $ Data.lookup label l
  let tstate = (Ready, Nothing, [seq])
  let tstates' = Data.insert tid tstate tstates
  put (pm, (tid, p, i, l, tstates', s))
evalStmt (StmtLabel _ s) = evalStmt s
evalStmt (StmtCall ident vars) = do
  (pm, (tid, p, i, l, tstates, s)) <- get
  let (Iline _ param seq) = fromJust $ find (\(Iline id _ _) -> ident == id) i
  let subs = fmap (\(AnyExprVarRef v1, AnyExprVarRef v2) -> (v1, v2)) (zip param vars)
  let seq' = replaceVars subs seq
  evalSequence seq'
  iterateSeq
   
-- Hack of the century right here
-- When considering inlines, we want to alias one variable to another
-- Going through an entire ADT to change this would be painful
-- So we just show the ADT and text replace the variable to its alias
replaceVars :: [(VarRef, VarRef)] -> Sequence -> Sequence
replaceVars [] seq = seq
replaceVars ((pre, replace):xs) seq = replaceVars xs (replaceVar pre replace seq)

replaceVar :: VarRef -> VarRef -> Sequence -> Sequence
replaceVar v1 v2 seq = 
  read (Data.Text.unpack $ Data.Text.replace 
      (Data.String.fromString . show $ v1) 
      (Data.String.fromString . show $ v2) 
      (Data.String.fromString . show $ seq))


evalPrint :: Pargs -> ProgState String
evalPrint (PArgsNoString []) = return []
evalPrint (PArgsBoth "" []) = return []
evalPrint (PArgsBoth str []) = return str
evalPrint (PArgsString str) = return str
evalPrint (PArgsNoString (e:es)) = do
  e' <- evalAnyExpr e
  let arg = case e' of  
        (Bl a) -> show a
        (By a) -> show a
        (Sh a) -> show a
        (I a) -> show a
        (UI a b) -> show a
        (S a) -> show a
  val <- evalPrint (PArgsNoString es)
  return $ arg ++ val
evalPrint (PArgsBoth str (e:es)) = do
  e' <- evalAnyExpr e
  let arg = case e' of  
        (Bl a) -> show a
        (By a) -> show a
        (Sh a) -> show a
        (I a) -> show a
        (UI a b) -> show a
        (S a) -> show a
  case elemIndex '%' str of
    (Just idx) -> do 
      let s = take idx str ++ arg
      end <- evalPrint (PArgsBoth (drop (idx+2) str) es)
      return $ s ++ end
    Nothing -> return []

-- Promela has a notion of executability
-- This will check if a statement is executable
executableStmt :: Stmt -> ProgState Bool
executableStmt (StmtIf opts) = executableOpts opts
executableStmt (StmtDo opts) =  executableOpts opts
executableStmt (StmtFor _ seq) = executableSeq seq
executableStmt (StmtAtomic atm) = executableSeq atm
executableStmt (StmtDAtomic atm) = executableSeq atm
executableStmt (StmtSelect _) = return True
executableStmt (StmtNorm seq) = executableSeq seq
executableStmt (StmtSend _) = return True
executableStmt (StmtRec _) = return True
executableStmt (StmtAssign _) = return True
executableStmt StmtElse = return True
executableStmt StmtBreak = return True
executableStmt (StmtGoto _) = return True
executableStmt (StmtLabel _ _) = return True
executableStmt (StmtPrint _ _) = return True
executableStmt (StmtAssert _) = return True
executableStmt (StmtCall _ _) = return True
executableStmt (StmtExpr (ExprAny (AnyExprRun {}))) = return True
executableStmt (StmtExpr e) = do
    val <- evalExpr e
    case val of 
        (Bl a) -> return a
        (By 0) -> return False
        (Sh 0) -> return False
        (I 0) -> return False
        (UI 0 b) -> return False
        (Bs 0) -> return False
        _ -> return True

-- Some statements mention sequences, so this evaluates those    
executableSeq :: Sequence -> ProgState Bool
executableSeq (SeqOne step) = executableStep step
executableSeq (SeqNoSep step _) = executableStep step
executableSeq (SeqOneSep step _) = executableStep step
executableSeq (SeqCons step _ _) = executableStep step

-- Some sequences mention steps, this evaluates those
executableStep :: Step -> ProgState Bool
executableStep (StepMType _)  = return True
executableStep (StepDclList _)  = return True
executableStep (StepXR _)  = return True
executableStep (StepXS _)  = return True
executableStep (StepStmt stmt _) = executableStmt stmt

-- Opts are used for if/do statements, as long as their disjunction is executable, they are too
executableOpts :: Options -> ProgState Bool
executableOpts (OptionsOne seq) = executableSeq seq
executableOpts (OptionsCons seq opts) = do
    seq <- executableSeq seq
    opts <- executableOpts opts
    return $ seq || opts

-- EVALUATORS
-- evalExpr is self-explantory
evalExpr :: Expr -> ProgState Vars
evalExpr (ExprAny e) = evalAnyExpr e
evalExpr (ExprParen e) = evalExpr e

evalBinOpNum :: (Bits a, Integral a) => (Int -> Int -> a) -> AnyExpr -> AnyExpr -> ProgState Vars
evalBinOpNum f e1 e2 = do
  e1' <- evalAnyExpr e1
  e2' <- evalAnyExpr e2
  return $ evalOpNum f e1' e2'

evalBinOpBool :: (Int -> Int -> Bool) -> AnyExpr -> AnyExpr -> ProgState Vars
evalBinOpBool f e1 e2 = do
  e1' <- evalAnyExpr e1
  e2' <- evalAnyExpr e2
  return $ evalOpBool f e1' e2'

dclIdents :: DeclList -> [(PIdent, Typename)]
dclIdents (DclListOneNoSep dcl) =
  case dcl of 
    (DclOne a typename ivar) -> map (\(Ivar ident _ _) -> (ident, typename)) ivar
    (DclOneUnsigned _ (UDcl ident _ _)) -> [(ident, TypenamePIdent . PIdent $ "unsigned")]
dclIdents (DclListOne dcl _) = dclIdents (DclListOneNoSep dcl)
dclIdents (DclListCons dcl _ dcls) = dclIdents (DclListOneNoSep dcl) ++ dclIdents dcls

-- Evaluates anyExpr, only difficult part is further down
evalAnyExpr :: AnyExpr -> ProgState Vars
evalAnyExpr (AnyExprCond c p q) = do
    c' <- evalAnyExpr c 
    case c' of 
        (Bl a)
            | a -> evalAnyExpr p
            | otherwise -> evalAnyExpr q
        _ -> error "Conditional expression does not evaluate to boolean"
evalAnyExpr (AnyExprland e1 e2) = do
    e1' <- evalAnyExpr e1
    e2' <- evalAnyExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a && b
evalAnyExpr (AnyExprlor e1 e2) = do
    e1' <- evalAnyExpr e1
    e2' <- evalAnyExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a || b
evalAnyExpr (AnyExprbitand e1 e2) = evalBinOpNum ((.&.) :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprbitor e1 e2) = evalBinOpNum ((.|.) :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprbitxor e1 e2) = evalBinOpNum (xor :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExpreq e1 e2) = do
    e1' <- evalAnyExpr e1
    e2' <- evalAnyExpr e2
    case (e1', e2') of 
        (Bl a, Bl b) -> return . Bl $ a == b
        _ -> return $ evalOpBool ((==) :: Int -> Int -> Bool) e1' e2'
evalAnyExpr (AnyExprneq e1 e2) = do
    (Bl a) <- evalAnyExpr (AnyExpreq e1 e2)
    return $ Bl . not $ a
evalAnyExpr (AnyExprlthan e1 e2) = evalBinOpBool ((<) :: Int -> Int -> Bool) e1 e2
evalAnyExpr (AnyExprgrthan e1 e2) = evalBinOpBool ((>) :: Int -> Int -> Bool) e1 e2
evalAnyExpr (AnyExprge e1 e2) = evalBinOpBool ((>=) :: Int -> Int -> Bool) e1 e2
evalAnyExpr (AnyExprle e1 e2) = evalBinOpBool ((<=) :: Int -> Int -> Bool) e1 e2
evalAnyExpr (AnyExprleft e1 e2) = evalBinOpNum (shift :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprright e1 e2) = do
    e1' <- evalAnyExpr e1
    e2' <- evalAnyExpr e2
    return $ evalOpNum (shift :: Int -> Int -> Int) e1' (evalUnrOp UnrOp2 e2')
evalAnyExpr (AnyExprplus e1 e2) = evalBinOpNum ((+) :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprminus e1 e2) = evalBinOpNum ((-) :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprtimes e1 e2) = evalBinOpNum ((*) :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprdiv e1 e2) = evalBinOpNum (div :: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprmod e1 e2) = evalBinOpNum (mod:: Int -> Int -> Int) e1 e2
evalAnyExpr (AnyExprUnrOp op e) = do
    e' <- evalAnyExpr e
    return $ evalUnrOp op e'

evalAnyExpr (AnyExprConst const) = do
    case const of 
        Const_true -> return . Bl $ True
        Const_false -> return . Bl $ False
        (ConstInteger int) -> return . I $ fromIntegral int
        Const_skip -> return . Bl $ True 

-- Simplest case, struct reference is just a reference to a variable
evalAnyExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone VarRefTypedefNone)) = do 
    ((local, global), cm@(tid, _, _, _, _, _)) <- get
    case Data.lookup id (fromJust $ Data.lookup tid local) of -- check locals
        (Just (v, _)) -> return v
        Nothing -> case Data.lookup id global of -- Check globals
            (Just (v, _)) -> return v
            Nothing -> error $ "Variable " ++ show id ++ "does not exist" ++ show cm-- then cry and give up

evalAnyExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone )) = do -- Now it's a reference to an array
    ((local, global), cm@(tid, _, _, _, _, _)) <- get
    e' <- evalAnyExpr e -- so evaluate the array index
    let idx :: Int = case e' of -- cast to an int
            (UI a b) -> fromIntegral a
            (I a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (By a) -> fromIntegral a
    case Data.lookup id (fromJust $ Data.lookup tid local) of -- look for it locally
      (Just (v, _)) -> listIndex v idx
      Nothing -> case Data.lookup id global of -- look for the array globally
        (Just (v, _)) -> listIndex v idx
        Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then give up again
evalAnyExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr))) = do -- We are now looking for an element of a struct
    struct <- evalAnyExpr (AnyExprVarRef (VarRef id VarRefAnyExprNone VarRefTypedefNone)) -- so first find the struct
    evalAnyExpr (AnyExprStructRef struct vr) -- Then find the element

evalAnyExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr))) = do -- We are looking for an element in a struct, in an array
    struct <- evalAnyExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone)) -- so first index the array for the struct
    evalAnyExpr (AnyExprStructRef struct vr) -- Then find the element in the struct
    
evalAnyExpr (AnyExprStructRef struct (VarRef id VarRefAnyExprNone VarRefTypedefNone)) = -- Logic for finding elements in a struct
    case (struct, id) of -- lookup the element in the struct
        (S m, id) ->  return . fromJust $ Data.lookup id m -- return it
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalAnyExpr (AnyExprStructRef struct (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr))) =
    case (struct, id) of
        (S m, id) ->  evalAnyExpr (AnyExprStructRef (fromJust $ Data.lookup id m) vr) -- look up the element in the struct and recursively search using variable reference
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalAnyExpr (AnyExprStructRef struct (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr))) = do -- we are looking for an element of an element of a struct that is itself an element of a list 
    e' <- evalAnyExpr e -- evaluate index
    let idx :: Int = case e' of
            (UI a b) -> fromIntegral a
            (I a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (By a) -> fromIntegral a
    case (struct, id) of -- look up the 
        (S m, id) -> do
             lv <- listIndex (fromJust $ Data.lookup id m) idx
             evalAnyExpr (AnyExprStructRef lv vr)
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalAnyExpr (AnyExprStructRef struct (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone)) = do -- looking for an element of a struct that is itself an element of a list
    e' <- evalAnyExpr e
    let idx :: Int = case e' of
            (UI a b) -> fromIntegral a
            (I a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (By a) -> fromIntegral a
    case (struct, id) of
        (S m, id) -> listIndex (fromJust $ Data.lookup id m) idx
        _ -> error $ "struct " ++ show struct ++ " does not contain member " ++ show id

evalAnyExpr (AnyExprRun ident args _) = do
  ((threads, g), c@(tid, p, i, l, tstates, s)) <- get
  let arg_lst = case args of
        RunArgsNone -> []
        (RunArgsOne es) -> do
          map evalAnyExpr es
  let tid' =  (+1) $ foldl max 0 (map fst (Map.toList tstates)) 
  args' <- GHC.Base.sequence (arg_lst ++ [pure . I . fromIntegral $ tid'])
  let ptype = fromJust $ find (\(Ptype _ id _ _ _ _) -> id == ident) p 
  let (vars, seq) = case ptype of (Ptype _ _ vars _ _ seq) -> (vars, seq)
  let (I pr, _) = fromJust $ Data.lookup (PIdent "_nr_pr") g
  let globals' = Data.insert (PIdent "_nr_pr") (I (pr+1), Typename_int) g
  let dcls = case vars of 
        PdeclListNone -> [(PIdent "_pid", Typename_pid)]
        (PdeclListOne dcl) -> dclIdents dcl ++ [(PIdent "_pid", Typename_pid)]
  let locals = setMem (zip args' dcls) 
  let threads' = Data.insert tid' locals threads
  let tstates' = Data.insert tid' (Ready, Nothing, [seq]) tstates
  put ((threads', globals'), (tid, p, i, l, tstates', s))
  return (I 0)
  where 
    setMem m = setMem' m Map.empty
    setMem' [] = id
    setMem' ((val, (ident, typename)):xs) = Map.insert ident (val, typename) <$> setMem' xs


updateVar :: VarRef -> Vars -> ProgState ()
updateVar (VarRef id VarRefAnyExprNone VarRefTypedefNone) val = do -- Simplest case, struct reference is just a reference to a variable
  ((threads, global), cm@(tid, _, _, _, _, _)) <- get
  let locals = fromJust $ Data.lookup tid threads
  case Data.lookup id locals of -- then check locals
    (Just (v, typename)) -> do 
      let locals' = Data.insert id (val, typename) locals
      let threads' = Data.insert tid locals' threads
      put ((threads', global), cm)
    Nothing -> case Data.lookup id global of -- Check globals
      (Just (v, typename)) -> put ((threads, Data.insert id (val, typename) global), cm)
      Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then cry and give up
updateVar (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr)) val = do
  ((threads, global), cm@(tid, _, _, _, _, _)) <- get
  let locals = fromJust $ Data.lookup tid threads
  case Data.lookup id locals of -- Check locals
      (Just (v, typename)) -> do 
        val' <- updateStruct vr v val
        let locals' = Data.insert id (val', typename) locals
        let threads' = Data.insert tid locals' threads
        put ((threads', global), cm)
      Nothing -> case Data.lookup id global of -- then check globals
        (Just (v, typename)) -> do
          val' <- updateStruct vr v val
          put ((threads, Data.insert id (val', typename) global), cm)
        Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then cry and give up
updateVar (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone) val = do
  ((threads, global), cm@(tid, _, _, _, _, _)) <- get
  e' <- evalAnyExpr e -- so evaluate the array index
  let idx :: Int = case e' of -- cast to an int
          (UI a b) -> fromIntegral a
          (I a) -> fromIntegral a
          (Sh a) -> fromIntegral a
          (By a) -> fromIntegral a
  let locals = fromJust $ Data.lookup tid threads
  case Data.lookup id locals of -- check locals
    (Just (v, typename)) -> do 
      list' <- listInsert v val idx
      let locals' = Data.insert id (list', typename) locals
      let threads' = Data.insert tid locals' threads
      put ((threads', global), cm)
    Nothing -> case Data.lookup id global of -- then look for the array globally
      (Just (v, typename)) -> do 
        list' <- listInsert v val idx
        put ((threads, Data.insert id (list', typename) global), cm)
      Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then cry and give up
updateVar (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr)) val = do
    ((threads, global), cm@(tid, _, _, _, _, _)) <- get
    e' <- evalAnyExpr e -- so evaluate the array index
    let idx :: Int = case e' of -- cast to an int
            (UI a b) -> fromIntegral a
            (I a) -> fromIntegral a
            (Sh a) -> fromIntegral a
            (By a) -> fromIntegral a
    struct <- evalAnyExpr (AnyExprVarRef (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone))
    struct' <- updateStruct vr struct val
    let locals = fromJust $ Data.lookup tid threads
    case Data.lookup id locals of -- then check locals
        (Just (v, typename)) -> do 
          list' <- listInsert v struct' idx
          let locals' = Data.insert id (list', typename) locals
          let threads' = Data.insert tid locals' threads
          put ((threads', global), cm)
        Nothing -> case Data.lookup id global of -- look for the array globally
          (Just (v, typename)) -> do 
            list' <- listInsert v struct' idx
            put ((threads, Data.insert id (list', typename) global), cm)
          Nothing -> error $ "Variable " ++ show id ++ "does not exist" -- then cry and give up

updateStruct :: VarRef -> Vars -> Vars -> ProgState Vars
updateStruct (VarRef id VarRefAnyExprNone VarRefTypedefNone) (S struct) var = return . S $ Data.insert id var struct
updateStruct (VarRef id (VarRefAnyExprOne e) VarRefTypedefNone) (S struct) var = do 
  e' <- evalAnyExpr e -- so evaluate the array index
  let idx :: Int = case e' of -- cast to an int
          (UI a b) -> fromIntegral a
          (I a) -> fromIntegral a
          (Sh a) -> fromIntegral a
          (By a) -> fromIntegral a
  case Data.lookup id struct of 
    (Just list) -> do 
      list' <- listInsert list var idx
      return . S $ Data.insert id list' struct
    Nothing -> error $ "Field: " ++ show id ++ " does not exist in struct " ++ show struct
updateStruct (VarRef id VarRefAnyExprNone (VarRefTypedefOne vr)) (S struct) var = do
  case Data.lookup id struct of 
    (Just field) -> do
      field' <- updateStruct vr field var
      return . S $ Data.insert id field' struct
    Nothing -> error $ "Field: " ++ show id ++ " does not exist in struct " ++ show struct
updateStruct (VarRef id (VarRefAnyExprOne e) (VarRefTypedefOne vr)) (S struct) var = do
  e' <- evalAnyExpr e -- so evaluate the array index
  let idx :: Int = case e' of -- cast to an int
          (UI a b) -> fromIntegral a
          (I a) -> fromIntegral a
          (Sh a) -> fromIntegral a
          (By a) -> fromIntegral a
  case Data.lookup id struct of 
    (Just list) -> do 
      listElem <- listIndex list idx
      listElem' <- updateStruct vr listElem var
      list' <- listInsert list listElem' idx
      return . S $ Data.insert id list' struct
    Nothing -> error $ "Field: " ++ show id ++ " does not exist in struct " ++ show struct
-- Underneath this is bad stuff that I've bodged together to get this to work. Just ignore it.
-- If you really have to know, it's fun type coercion stuff

-- Strip off the type constructor to index a list
listIndex :: Vars -> Int -> ProgState Vars
listIndex vars idx = case vars of
    (BlL v) -> return . Bl $ v !! idx
    (ByL v) -> return . By $ v !! idx
    (ShL v) -> return . Sh $ v !! idx
    (IL v) -> return . I $ v !! idx
    (UIL v b) -> return $  UI (v !! idx) b
    (SL v) -> return . S $ v !! idx

listInsert :: Vars -> Vars -> Int -> ProgState Vars
listInsert list var idx = do 
  case (list, var) of 
    (BlL vs, Bl v) -> return . BlL $ take (idx-1) vs ++ [v] ++ drop idx vs
    (SL vs, S v) -> return . SL $ take (idx-1) vs ++ [v] ++ drop idx vs
    (ByL vs, By v) -> return . ByL $ take (idx-1) vs ++ [v] ++ drop idx vs
    (ByL vs, Sh v) -> return . ByL $ take (idx-1) vs ++ [fromIntegral v] ++ drop idx vs
    (ByL vs, I v) -> return . ByL $ take (idx-1) vs ++ [fromIntegral v] ++ drop idx vs
    (ByL vs, UI v b) -> return . ByL $ take (idx-1) vs ++ [fromIntegral $ v .&. (2^8)-1] ++ drop idx vs
    (ShL vs, By v) -> return . ShL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral v] ++ drop (fromIntegral idx) vs
    (ShL vs, Sh v) -> return . ShL $ take (fromIntegral $ idx-1) vs ++ [v] ++ drop (fromIntegral idx) vs
    (ShL vs, I v) -> return . ShL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral v] ++ drop (fromIntegral idx) vs
    (ShL vs, UI v b) -> return . ShL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral $ v .&. (2^16)-1] ++ drop (fromIntegral idx) vs
    (IL vs, By v) -> return . IL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral v] ++ drop (fromIntegral idx) vs
    (IL vs, Sh v) -> return . IL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral v] ++ drop (fromIntegral idx) vs
    (IL vs, I v) -> return . IL $ take (fromIntegral $ idx-1) vs ++ [v] ++ drop (fromIntegral idx) vs
    (IL vs, UI v b) -> return . IL $ take (fromIntegral $ idx-1) vs ++ [fromIntegral $ v .&. (2^32)-1] ++ drop (fromIntegral idx) vs
    (UIL vs b1, UI v b2) -> let mval = (2^(max b1 b2))-1 in 
      return $ UIL ((map ((.&.) mval) (take (fromIntegral $ idx-1) vs)) ++ [v] ++ (map ((.&.) mval) (drop (fromIntegral idx) vs))) (max b1 b2) 
    (a, b) -> error $ "could not insert into list, type mismatch: " ++ show a ++ " " ++ show b 

evalUnrOp :: UnrOp -> Vars -> Vars
evalUnrOp UnrOp3 (Bl a) = Bl $ not a
evalUnrOp UnrOp1 (By a) = By $ complement a
evalUnrOp UnrOp2 (By a) = By $ - a
evalUnrOp UnrOp3 (By 0) = By 1 
evalUnrOp UnrOp3 (By a) = By 0 
evalUnrOp UnrOp1 (Sh a) = Sh $ complement a
evalUnrOp UnrOp2 (Sh a) = Sh $ - a
evalUnrOp UnrOp3 (Sh 0) = Sh 1 
evalUnrOp UnrOp3 (Sh a) = Sh 0 
evalUnrOp UnrOp1 (I a) = I $ complement a
evalUnrOp UnrOp2 (I a) = I $ - a
evalUnrOp UnrOp3 (I 0) = I 1 
evalUnrOp UnrOp3 (I a) = I 0 
evalUnrOp UnrOp1 (UI a b) = UI (complement a .&. (2^b)-1) b
evalUnrOp UnrOp2 (UI a b) = UI (- (a .&. (2^b)-1)) b
evalUnrOp UnrOp3 (UI 0 b) = UI 1 b
evalUnrOp UnrOp3 (UI a b) = UI 0 b
evalUnrOp UnrOp1 (Bs a) = Bs $ complement a
evalUnrOp UnrOp2 (Bs a) = Bs $ - a
evalUnrOp UnrOp3 (Bs 0) = Bs 1 
evalUnrOp UnrOp3 (Bs a) = Bs 0 

-- HACKINESS ENSUES
-- We want to have our applyOp function actually work, so here goes
-- We promite a and b to Integers, then apply the op
applyOp :: (Bits a1, Integral a1, Bits a2, Integral a2, Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> t3) -> a1 -> a2 -> t3
applyOp op a b = fromIntegral a `op` fromIntegral b 

-- We applyOp and apply the correct type constructor (a byte added to an int should be an int)
evalOpNum :: (Bits a, Integral a, Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> a) -> Vars -> Vars -> Vars
evalOpNum op (By a) (By b) = By . fromIntegral $ applyOp op a b
evalOpNum op (By a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (By a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (By a) (UI b bits) = UI ((fromIntegral $ applyOp op a b) .&. ((2^(max bits 8))-1)) (max bits 8)
evalOpNum op (Sh a) (By b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (Sh b) = Sh . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (Sh a) (UI b bits) = UI ((fromIntegral $ applyOp op a b) .&. ((2^(max bits 16))-1)) (max bits 16)
evalOpNum op (I a) (By b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (Sh b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (I a) (UI b bits) = I . fromIntegral $ applyOp op a b
evalOpNum op (UI a bits) (By b) = UI ((fromIntegral $ applyOp op a b) .&. ((2^(max bits 8))-1)) (max bits 8)
evalOpNum op (UI a bits) (Sh b) = UI ((fromIntegral $ applyOp op a b) .&. ((2^(max bits 16))-1)) (max bits 16)
evalOpNum op (UI a bits) (I b) = I . fromIntegral $ applyOp op a b
evalOpNum op (UI a bits) (UI b bits') = UI ( (fromIntegral $ applyOp op a b) .&. (2^(max bits bits')-1)) (max bits bits')
evalOpNum op (Bs a) (Bs b) = Bs . fromIntegral $ applyOp op a b
evalOpNum op a b = error $ "No operation can be performed with operators: " ++ show a ++ " " ++ show b

-- Same as above but with binary boolean operators
evalOpBool :: (Bits t1, Num t1, Bits t2, Num t2) => (t1 -> t2 -> Bool) -> Vars -> Vars -> Vars
evalOpBool op (By a) (By b) = Bl $ applyOp op a b
evalOpBool op (By a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (By a) (I b) = Bl $ applyOp op a b
evalOpBool op (By a) (UI b bits) = Bl $ applyOp op a b
evalOpBool op (Sh a) (By b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (I b) = Bl $ applyOp op a b
evalOpBool op (Sh a) (UI b bits) = Bl $ applyOp op a b
evalOpBool op (I a) (By b) = Bl $ applyOp op a b
evalOpBool op (I a) (Sh b) = Bl $ applyOp op a b
evalOpBool op (I a) (I b) = Bl $ applyOp op a b
evalOpBool op (I a) (UI b bits) = Bl $ applyOp op a b
evalOpBool op (UI a bits) (By b) = Bl $ applyOp op a b
evalOpBool op (UI a bits) (Sh b) = Bl $ applyOp op a b
evalOpBool op (UI a bits) (I b) = Bl $ applyOp op a b
evalOpBool op (UI a bits) (UI b bits') = Bl $ applyOp op a b
evalOpBool op (Bs a) (Bs b) = Bl $ applyOp op a b