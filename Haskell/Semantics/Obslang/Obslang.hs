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
import Control.Monad.Trans.Writer
import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (MonadTrans(lift))

type Mem = Map.Map Varname (Vars, Typename)
type ProgState a = StateT ( Map.Map ThreadId ( Mem, StateObs ), Mem, ThreadId ) (WriterT [String] IO) a

funcMap = Map.fromList [("update1", update1), ("update2", update2)] -- Change as needed
data ObsList = ObsListCons Obs ObsList | ObsListOne Obs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Vars = S Varval 
    | LInt [Integer] 
    | LString [String] 
    | LDouble [Double]
  deriving Show

data ObsId = ObsId ThreadId Obs
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Obs
    = ObsName ObsIdent
    | ObsInit
    | ObsTask Taskname
    | ObsSignal Integer
    | ObsDef Varname Varval
    | ObsDecl Typename Varname
    | ObsDeclVal Typename Varname Varval
    | ObsDeclArr Typename Varname SizeDcl
    | ObsCall ObsIdent Args
    | ObsState Integer StateObs
    | ObsStruct Varname
    | ObsSeq Varname Scalar
    | ObsPtr Varname Varval
    | ObsScalar Varname Varval
    | ObsScalarIndex Varname Varindex Varval
    | ObsEnd Varname
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

execObs :: ObsId -> ProgState ()
execObs (ObsId tid obs) = do 
  (threads, global, prev_tid) <- get
  case obs of
    (ObsDecl type_dcl ident)
      | type_dcl `elem` map tname ["int", "short", "long", "long long" ] ->
        put (threads, Data.insert ident (S (VarValInt 0), type_dcl) global, tid)
      | type_dcl `elem` map tname ["float", "double" ] ->
        put (threads, Data.insert ident (S (VarValDouble 0.0), type_dcl) global, tid)
      | type_dcl `elem` map tname ["char *", "char" ] ->
        put (threads, Data.insert ident (S (VarValStr . ObsStr $ ""), type_dcl) global, tid)
      | otherwise ->
        put (threads, Data.insert ident (S VarValNull, type_dcl) global, tid)
    (ObsDeclVal type_dcl ident val) -> do
      put (threads, Data.insert ident (S val, type_dcl) global, tid)
    (ObsPtr ident val) -> do
      put (threads, Data.insert ident (S val, TypeName $ ObsIdent "PTR") global, tid)
      Control.Monad.when (tid /= prev_tid) (obsLog $ "thread change: " ++ show prev_tid ++ " to " ++ show tid)   
    (ObsState task tstate) -> do
      let (local_vars, _) = fromJust $ Data.lookup tid threads 
      put (Data.insert tid (local_vars, tstate) threads, global, tid)
    (ObsTask _) -> do
      put (Data.insert tid (Map.empty, StateObs $ ObsIdent "Ready") threads, global, tid)
    (ObsScalar ident oVal) -> do
      case Data.lookup ident global of 
        (Just (S mVal, typename)) -> when (oVal /= mVal)  
          (obsLog $ "Error, oVal /= mVal: oVal: " ++ show oVal ++ " mVal: " ++ show mVal ++ " for ident: " ++ show ident)
        Nothing -> obsLog $ "Error, " ++ show ident ++ " could not be found in memory"
        _ -> obsLog $ "Error, " ++ show ident ++ " has mismatched types"
    (ObsScalarIndex ident (VarIndex idx) val) -> do
      case Data.lookup ident global of
            Just (list, vtype) -> 
              do 
                case (list, val) of 
                  (LInt xs, VarValInt val) -> checkList (fromIntegral idx) val xs
                  (LString xs, VarValStr (ObsStr val)) -> checkList (fromIntegral idx) val xs
                  (LDouble xs, VarValDouble val) -> checkList (fromIntegral idx) val xs 
            _ -> obsLog $ "Lookup returned nothing for: " ++ show ident 
    (ObsCall (ObsIdent fname) args) -> do
      case Map.lookup fname funcMap of
        (Just func) -> func args
        Nothing -> obsLog $ "No function with name" ++ show fname ++ "could be found"
    _ -> return ()
  monitorThreadHandover tid prev_tid
  where
    checkList :: (Eq a, Show a) => Int -> a -> [a] -> ProgState ()
    checkList idx val xs
      | length xs >= idx = obsLog $ "List is too short for index: idx: " ++ show idx ++ "len list: " ++ show (length xs) 
      | xs !! idx /= val = obsLog $ "List elem is not equal to value supplied: list elem: " ++ show (xs !! idx) ++ "val " ++ show val 
      | otherwise = return ()
    updateList constr idx val xs = 
      constr $ take (fromIntegral $ idx-1) xs ++ [val] ++ drop (fromIntegral idx) xs
    monitorThreadHandover tid prev_tid =
      when (tid /= prev_tid) (obsLog $ "thread change: " ++ show prev_tid ++ " to " ++ show tid)   

execListObs :: [ObsId] -> ProgState ()
execListObs [] = return ()
execListObs (x:xs) = do 
  execObs x
  execListObs xs

runProgState :: [ObsId] -> IO ()
runProgState prog = do
  st <- runWriterT $ (runStateT  $ execListObs prog) (Map.empty, Map.empty, (ThreadId 0))
  case st of 
    (((), (threads, vars, tid)), log) -> putStr $ 
      "Threads: " ++ show threads ++ "\n" ++
      "Final threadId: " ++ show tid ++ "\n" ++
      "Vars: " ++ show vars ++ "\n" ++
      "Log: " ++ show log

update1 :: Args -> ProgState ()
update1 _ = do
  (local, global, tid) <- get
  case (Data.lookup (vname "g1") global, Data.lookup (vname "g2") global) of
    (Just (S (VarValInt g1_val), g1_type), Just (S (VarValInt g2_val), g2_type)) -> do
      let g1_val = g2_val + 10
      let g2_val = g1_val * 2
      let update = Data.insert (vname "g1") (S . VarValInt $ g1_val, g1_type) <$>
                   Data.insert (vname "g2") (S . VarValInt $ g2_val, g2_type)
      put (local, update global, tid)
    (Nothing, Nothing) -> obsLog "g1 and g2 not found in memory"
    (Nothing, _) -> obsLog "g1 not found in memory"
    (_, Nothing) -> obsLog "g2 not found in memory"
    (_, _) -> obsLog "Type error with g1/g2"


update2 :: Args -> ProgState ()
update2 _ = do
  (local, global, tid) <- get
  case (Data.lookup (vname "g1") global, Data.lookup (vname "g2") global) of
    (Just (S (VarValInt g1_val), g1_type), Just (S (VarValInt g2_val), g2_type)) -> do
      let g2_val = g1_val + 5 
      let g1_val = g2_val * 3
      let update = Data.insert (vname "g1") (S . VarValInt $ g1_val, g1_type) <$> 
                   Data.insert (vname "g2") (S . VarValInt $ g2_val, g2_type)
      put (local, update global, tid)
    (Nothing, Nothing) -> obsLog $ "g1 and g2 not found in memory"
    (Nothing, _) -> obsLog "g1 not found in memory"
    (_, Nothing) -> obsLog "g2 not found in memory"
    (_, _) -> obsLog "Type error with g1/g2"

vname = VarName . ObsIdent
tname = TypeName . ObsIdent

obsLog :: String -> ProgState ()
obsLog l = lift $ tell [l]