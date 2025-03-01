{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}

-- | The abstract syntax of language Obs.
import qualified Data.Map as Data

import Control.Monad.Trans.State ( get, put, StateT(runStateT) )
import qualified Data.Eq as C
import qualified Data.Ord as C
import qualified GHC.Prelude as C
import qualified Data.String
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import qualified Data.Map as Map

type Mem = Map.Map Varname (Vars, Typename)
type ProgState a = StateT ( Map.Map ThreadId ( Mem, StateObs ), Mem ) IO a

data ObsList = ObsListCons Obs ObsList | ObsListOne Obs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Vars = S Varval 
    | LInt [Integer] 
    | LString [String] 
    | LDouble [Double]
  deriving Show

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

execObs :: Obs -> ProgState ()
execObs obs = do 
  st <- get
  case obs of
    (ObsDecl _ type_dcl ident) ->
      put (fst st, Data.insert ident (S VarValNull, type_dcl) (snd st))
    (ObsDeclVal tid type_dcl ident val) ->
      put (fst st, Data.insert ident (S val, type_dcl) (snd st))
    (ObsPtr _ ident val) ->
      put (fst st, Data.insert ident (S val, TypeName $ ObsIdent "PTR") (snd st))
    (ObsState tid task tstate) -> do
      let local_vars = fst . fromJust $ Data.lookup tid (fst st) 
      put (Data.insert tid (local_vars, tstate) (fst st), (snd st))
    (ObsTask tid _) -> do
      put (Data.insert tid (Map.empty, StateObs $ ObsIdent "Ready") (fst st), (snd st))
    (ObsScalar _ ident val) -> do
      let vtype = snd . fromJust $ Data.lookup ident (snd st) 
      put (fst st, Data.insert ident (S val, vtype) (snd st))
    (ObsScalarIndex _ ident (VarIndex idx) val) -> do
      let (list, vtype) = case (Data.lookup ident (snd st)) of
            Just (list, vtype) -> (list, vtype)
            _ -> error "Var not found"
      let update = case (list, val) of 
            (LInt xs, VarValInt val) -> updateList LInt idx val xs
            (LString xs, VarValStr (ObsStr val)) -> updateList LString idx val xs
            (LDouble xs, VarValDouble val) -> updateList LDouble idx val xs 
            _ -> error "List of non-list type"
      put (fst st, Data.insert ident (update, vtype) (snd st))
    _ -> return ()
  where
    updateList constr idx val xs = 
      constr $ take (fromIntegral $ idx-1) xs ++ [val] ++ drop (fromIntegral idx) xs

execListObs :: [Obs] -> ProgState ()
execListObs [] = return ()
execListObs (x:xs) = do 
  execObs x
  execListObs xs

runProgState :: [Obs] -> IO ()
runProgState prog = do
  st <- (runStateT $ execListObs prog) (Map.empty, Map.empty)
  case st of 
    ((), (threads, vars)) -> print $ "Threads: " ++ show threads ++ "Vars: " ++ show vars