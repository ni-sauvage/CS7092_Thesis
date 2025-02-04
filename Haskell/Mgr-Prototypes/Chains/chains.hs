data Node a = Node (Node a) (Node a) a Word | None

data Chain a = Chain (Node a) (Node a) Word


instance Show a => Show (Node a) where
    show :: Show a => Node a -> String
    show (Node _ _ val _) = "@@@ 0 STRUCT nptr\n\
                          \@@@ 0 SCALAR itm " ++ show val ++ "\n\
                          \@@@ 0 END nptr\n"

instance Show a => Show (Chain a) where
    show :: Show a => Chain a -> String
    show (Chain head tail size) = "@@@ 0 SEQ chain\n" ++ showChain head

showChain :: Show a => Node a -> String
showChain None = "@@@ 0 END chain\n"
showChain (Node nxt _ itm _) = "@@@ 0 SCALAR _ " ++ show itm ++ "\n" ++ showChain nxt

showNode :: Show a => Chain a -> Word -> String
showNode (Chain head _ _) nptr = "@@@ 0 PTR nptr " ++ show nptr ++ "\n" ++ if nptr /= 0 then showNode' head nptr else ""

showNode' :: Show a => Node a -> Word -> String
showNode' node@(Node nxt _ itm idx) nptr
    | nptr == idx = show node
    | otherwise = showNode' nxt nptr

appendNode :: Chain a -> Node a -> Chain a
appendNode (Chain None _ size) node = Chain node node 1
appendNode (Chain head tail size) node@(Node _ _ item idx) = Chain (appendNode' head node) (Node None tail item idx) (size+1)

appendNode' :: Node a -> Node a -> Node a
appendNode' chNode@(Node None chPrv chItm chIdx) (Node _ _ itm idx)  = Node (Node None chNode itm idx) chPrv chItm chIdx
appendNode' chNode@(Node chNxt chPrv chItem chIdx) inNode = Node (appendNode' chNxt inNode) chPrv chItem chIdx

doAppend :: Show a => Chain a -> Word -> a -> IO (Chain a)
doAppend ch ptr itm = do
    putStrLn $ "@@@ 0 CALL append "  ++ show itm ++ " "++ show ptr 
    let ch' = appendNode ch (Node None None itm ptr)
    putStr $ show ch'
    return ch'

initStr :: String
initStr = 
    "\n\n Chain Model running.\n\
    \@@@ 0 NAME Chain_AutoGen\n\
    \@@@ 0 DEF MAX_SIZE 8\n\
    \@@@ 0 DCLARRAY Node memory MAX_SIZE\n\
    \@@@ 0 DECL unsigned nptr NULL\n\
    \@@@ 0 DECL Control chain\n\
    \\nInitialising...\n\
    \@@@ 0 INIT\n"

getCh :: Show a => Chain a -> Word -> (Chain a, Word)
getCh (Chain (Node nxt _ _ idx) tail size) nptr = (Chain nxt tail (size-1), idx)


doGet :: Show a => Chain a -> Word -> IO (Chain a, Word)
doGet ch nptr = do
    let (ch', nptr') = getCh ch nptr
    putStrLn $ "@@@ 0 CALL getNonNull " ++ show nptr'
    putStr $ show ch'
    putStr $ showNode ch nptr'
    return (ch', nptr')


main :: IO ()
main = do
    let ch = Chain (None::Node Word) None 0
    putStr initStr
    putStr $ show ch
    let nptr = 0
    putStr $ showNode ch nptr
    ch <- doAppend ch 4 23
    (ch, nptr) <- doGet ch nptr
    ch <- doAppend ch 3 22
    (ch, nptr) <- doGet ch nptr
    ch <- doAppend ch 6 21
    doGet ch nptr
    return ()