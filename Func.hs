import Data.Maybe

-- All programs are assumed to be well-formed in the following sense:
--
-- All operators, functions and procedures will always be applied
-- to the correct number of arguments, all of which will be of the appropriate
-- type.
--
-- Boolean-valued expressions will always evaluate to either 0 (false) or 1
-- (true).
--
-- In an array element assignment the array being assigned to will always be 
-- in scope.
--
-- In a procedure call of the form Call x p es the procedure p will always exit 
-- via a Return statement.
--
-- A Return statement will always be the last statement to be executed in a 
-- procedure's defining code block (there is no `dead code').
--

--------------------------------------------------------------------
type Id = String

data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)

data Op = Add | Mul | Less | Equal | Index
          deriving (Eq, Show)

data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

type FunDef = (Id, ([Id], Exp))

type Block = [Statement]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type ProcDef = (Id, ([Id], Block))

data Scope = Local | Global
           deriving (Eq, Show)

type Binding = (Id, (Scope, Value))

type State = [Binding]

--------------------------------------------------------------------
-- Part I

getValue :: Id -> State -> Value
-- Pre: The identifier has a binding in the state
getValue id st
  = snd (lookUp id st)

getLocals :: State -> State
getLocals st
  = filter ((== Local) . fst . snd) st

getGlobals :: State -> State
getGlobals st
  = filter ((== Global) . fst . snd) st

assignArray :: Value -> Value -> Value -> Value
-- The arguments are the array, index and (new) value respectively
-- Pre: The three values have the appropriate value types (array (A), 
--      integer (I) and integer (I)) respectively.
assignArray (A as) (I i) (I v)
  = A ((i, v) : (filter ((/= i) . fst) as))

updateVar :: (Id, Value) -> State -> State
updateVar (id, v) []
  = [(id, (Local, v))]
updateVar (id, v) (b@(id', (s, _)) : st)
  | id == id' = (id, (s, v)) : st
  | otherwise = b : updateVar (id, v) st

---------------------------------------------------------------------
-- Part II

table = [(Add, (+)), (Mul, (*)), (Less, bVal (<)), (Equal, bVal (==))]
bVal p i1 i2
  = if (p i1 i2) then 1 else 0

applyOp :: Op -> Value -> Value -> Value
-- Pre: The values have the appropriate types (I or A) for each primitive
applyOp Index (A as) (I i)
  = maybe (I 0) (\i' -> I i') (lookup i as) 
applyOp op (I i1) (I i2)
  = I ((lookUp op table) i1 i2)

bindArgs :: [Id] -> [Value] -> State
-- Pre: the lists have the same length
bindArgs ids vals
--  = zip ids (zip (repeat Local) vals)
  = zipWith (\id val -> (id, (Local, val))) ids vals

evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs es fds st
  = map (\e -> eval e fds st) es

eval :: Exp -> [FunDef] -> State -> Value
-- Pre: All expressions are well formed
-- Pre: All variables referenced have bindings in the given state
eval e fds st 
  = case e of
  Const v 
    -> v
  Var id  
    -> getValue id st
  OpApp op e1 e2 
    -> applyOp op (eval e1 fds st) (eval e2 fds st)
  Cond p e1 e2 
    -> eval ep fds st
    where
     ep = if (eval p fds st == I 1) then e1 else e2
  FunApp f es 
    -> eval e fds (st' ++ st)
    where
     (ids, e) = lookUp f fds
     st'      = bindArgs ids (evalArgs es fds st)

---------------------------------------------------------------------
-- Part III

executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All statements are well formed 
-- Pre: For array element assignment (AssignA) the array variable is in scope,
--      i.e. it has a binding in the given state
executeStatement stm fds pds st
  = case stm of
  Assign id e 
    -> updateVar (id, eval e fds st) st 
  AssignA id e1 e2
    -> updateVar (id, valA) st
    where 
     valA = assignArray (getValue id st) (eval e1 fds st) (eval e2 fds st) 
  If p b1 b2 
    -> executeBlock bp fds pds st
    where 
     bp = if (eval p fds st == I 1) then b1 else b2     
  While p b -> case (eval p fds st) of
    I 1 -> executeStatement stm fds pds st'
    _   -> st
    where
     st' = executeBlock b fds pds st  
  Call id p es 
    | null id   -> st'
    | otherwise -> updateVar (id, getValue "$res" st') st'' 
    where
     (ids, b) = lookUp p pds
     st'      = executeBlock b fds pds stH'
     stH'     = bindArgs ids (evalArgs es fds st) ++ st
     st''     = getLocals st ++ getGlobals st'
  Return e 
    -> executeStatement (Assign "$res" e) fds pds st

executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All code blocks and associated statements are well formed
executeBlock b fds pds st
  = foldl exS st b
  where
   exS st' stm = executeStatement stm fds pds st'

--------------------------------------------------------------------
-- Part IV

translate :: FunDef -> Id -> [(Id, Id)] -> ProcDef
translate (name, (as, e)) newName nameMap 
  = (newName, (as, b ++ [Return e']))
  where
   (b, e', ids') = translate' e nameMap ['$' : show n | n <- [1..]] 

translate' :: Exp -> [(Id, Id)] -> [Id] -> (Block, Exp, [Id])
translate' e nameMap is
  = trans [e] [] (head is) is
  where
   trans [] stack i is       
     = (stack, Var i, is)
   trans (e : es) stack i is 
     = trans es (stack ++ b) i ids 
     where
      (b, i, ids) = case e of
        Const _
          -> ([Assign (head is) e], head is, tail is)
        Var _ 
          -> ([Assign (head is) e], head is, tail is)
        OpApp Index e1 e2
          -> ([AssignA (head is) e1 e2], head is, tail is)
        OpApp op e1 e2
          -> (stack1 ++ stack2 ++ stack', head is'', tail is'')
          where
           (stack1, Var i', is')   = trans [e1] [] i is
           (stack2, Var i'', is'') = trans [e2] [] i' is'
           stack' = [Assign (head is'') (OpApp op (Var i') (Var i''))]
        Cond p e1 e2 
          -> ([If p stack1' stack2], i'', is'')
          where
           (stack1, Var i', is')   = trans [e1] [] i is
           (stack2, Var i'', is'') = trans [e2] [] i' (i' : is')
           stack1'                 = matchIndex stack1 i''
        FunApp id es'
          -> ([Call (head is) (lookUp id nameMap) es'], head is, tail is)

matchIndex b i 
  = case b of
  []                -> []
  [Assign _ e]      -> [Assign i e]
  [AssignA _ id' e] -> [AssignA i id' e]
  [If p b1 b2]      -> [If p (matchIndex b1 i) (matchIndex b2 i)]
  [While p b']      -> [While p (matchIndex b' i)]
  [Call _ id' es]   -> [Call i id' es]
  (stm : b')        -> stm : matchIndex b' i

---------------------------------------------------------------------
-- PREDEFINED FUNCTIONS

-- A helpful predefined lookUp function...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t
  = fromMaybe (error ("\nAttempt to lookUp " ++ show x ++ 
                      " in a table that only has the bindings: " ++ 
                      show (map fst t))) 
              (lookup x t)

 -- Turns an int into an Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Turns a list of ints into an array Exp...
listToExp :: [Int] -> Exp
listToExp 
  = Const . listToVal

-- Turns a list of ints into an array Value...
listToVal :: [Int] -> Value
listToVal xs
  = A (zip [0..] xs)

-- memoise generates a procedure that caches values computed by function f.  
-- In general f will be a variant of some originally recursive function 
-- that calls the procedure generated here (named p) instead of itself.
-- Arguments:
--    p = procedure name; a = argument name; f = function variant; 
--    pt = 'isPresent' table; mt = memo table.

memoise :: Id -> Id -> Id -> Id -> Id -> ProcDef
memoise p a f pt mt
  = (p, 
     ([a], [If (OpApp Equal (OpApp Index (Var pt) (Var a)) (Const (I 0)))
               [Call "x" f [Var a],
                AssignA pt (Var a) (Const (I 1)),
                AssignA mt (Var a) (Var "x")
               ]
               [],
            Return (OpApp Index (Var mt) (Var a))
           ]
     )
    )


---------------------------------------------------------------------
-- Predefined States, arrays and expressions for testing...

sampleState, gState, fibState :: State
sampleState 
  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

gState 
  = [("gSum", (Global, I 0))]

fibState 
  = [("fibPres", (Global, A [])), ("fibTab", (Global, A []))]

sampleArray :: Exp
sampleArray 
  = Const (listToVal [9,5,7,1])

e1, e2, e3, e4, e5 :: Exp
e1 = Const (I 1)
e2 = Var "y"
e3 = OpApp Add (Var "x") (Const (I 2))
e4 = Cond e1 (Var "x") (Const (I 9))
e5 = FunApp "fib" [Const (I 6)]

---------------------------------------------------------------------
-- Example (pure) functions for testing...

-- Equivalent of Haskell's max function...
biggest :: FunDef
biggest
  = ("biggest",
     (["m", "n"], Cond (OpApp Less (Var "m") (Var "n"))
                       (Var "n")
                       (Var "m"))
    )

-- Factorial, equivalent to: if n == 0 then 1 else n * fact (n - 1)...
fac :: FunDef
fac
  = ("fac",
     (["n"], Cond (OpApp Equal (Var "n") (intToExp 0))
                  (intToExp 1)
                  (OpApp Mul (Var "n") 
                             (FunApp "fac" [OpApp Add (Var "n") (intToExp (-1))])))
    )

-- Sums elements 0..n of an array...
sumA :: FunDef
sumA
  = ("sumA",
     (["a", "n"], Cond (OpApp Less (Var "n") (Const (I 0)))
                       (Const (I 0))
                       (OpApp Add (OpApp Index (Var "a") (Var "n"))
                                  (FunApp "sumA"
                                     [Var "a", OpApp Add (Var "n")
                                                         (Const (I (-1)))]))
     )
    )


-- Vanilla Haskell fib
fibH :: Int -> Int
-- Pre: n > 0
fibH n 
  = if n < 3 then 1 else fibH (n-1) + fibH (n-2)

-- fib in the purely functional subset
fib :: FunDef
fib
  = ("fib",
     (["n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Add (Var "n") (Const (I (-1)))])
                             (FunApp "fib" [OpApp Add (Var "n") (Const (I (-2)))]))
     )
    )

-- May be useful for testing translate...?
testFun :: FunDef
testFun
  = ("testFun",
     (["x", "y"], Cond (OpApp Equal (Var "x") (Var "y"))
                       (Cond (FunApp "p" [Var "y"])
                             (OpApp Add (Var "x") (Const (I 1)))
                             (OpApp Add (Var "x") (Var "y")))
                       (OpApp Add (FunApp "g" [Var "y"]) (Const (I 2))))
    )

---------------------------------------------------------------------
-- Example procedures for testing...

-- Add two integers and assign the result to a global variable, gSum, 
-- that is assumed to be in scope when the procedure is called...
gAdd :: ProcDef
gAdd
  = ("gAdd", 
     (["x", "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Sums elements 0..n of an array...
sumA' :: ProcDef
sumA'
  = ("sumA'",
     (["a", "n"], [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- A procedural version of fib...
fibP :: ProcDef
-- Pre: n > 0
fibP 
  = ("fibP", 
     (["n"], [If (OpApp Less (Var "n") (Const (I 3))) 
                 [Return (Const (I 1))]
                 [Call "f1" "fibP" [OpApp Add (Var "n") (Const (I (-1)))], 
                  Call "f2" "fibP" [OpApp Add (Var "n") (Const (I (-2)))], 
                  Return (OpApp Add (Var "f1") (Var "f2"))
                 ]
             ]
     )
    )

fibManager :: ProcDef
fibManager
  = ("fibManager",
     (["n"], [If (OpApp Equal (OpApp Index (Var "fibPres") (Var "n")) 
                              (Const (I 0))) 
                 [Call "x" "fibM" [Var "n"], 
                  AssignA "fibPres" (Var "n") (Const (I 1)), 
                  AssignA "fibTab" (Var "n") (Var "x")
                 ] 
                 [], 
              Return (OpApp Index (Var "fibTab") (Var "n"))
             ]
     )
    )

fibM :: ProcDef
-- Pre: n > 0
-- The value of fibMGenerator below
fibM 
  = ("fibM", 
     (["n"], [If (OpApp Less (Var "n") (Const (I 3)))
                 [Assign "$3" (Const (I 1))]
                 [Call "$1" "fibManager" [OpApp Add (Var "n") (Const (I (-1)))],
                  Call "$2" "fibManager" [OpApp Add (Var "n") (Const (I (-2)))],
                  Assign "$3" (OpApp Add (Var "$1") (Var "$2"))
                 ],
              Return (Var "$3")
             ]
     )
    )

---------------------------------------------------------------------
-- Sample top-level calls for testing...

-- This instantiates the table manager template (predefined)...
fibTableManager :: ProcDef
fibTableManager 
  = memoise "fibManager" "n" "fibM" "fibPres" "fibTab"

-- This uses the translate function to build the procedural, memoised,
-- version of fib...
fibMGenerator :: ProcDef
fibMGenerator 
  = translate fib "fibM" [("fib", "fibManager")] 
                   

-- Useful predefined executors...

execBiggest :: Int -> Int -> State
execBiggest m n
  = executeBlock [Return (FunApp "biggest" [intToExp m, intToExp n])] [biggest] [] []

execFac :: Int -> State
execFac n
  = executeBlock [Return (FunApp "fac" [intToExp n])] [fac] [] []

execSumA :: [Int] -> Int -> State
execSumA a n
  = executeBlock [Return (FunApp "sumA" [listToExp a, intToExp n])] [sumA] [] []

execGAdd :: Int -> Int -> State
execGAdd x y
  = executeBlock [Call "" "gAdd" [intToExp x, intToExp y]] [] [gAdd] gState

execSumA' :: [Int] -> Int -> State
execSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]] [] [sumA'] []

execGlobalSumA' :: [Int] -> Int -> State
execGlobalSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]] 
                 [] [sumA'] [("s", (Global, I 0))]

execFibP :: Int -> State
execFibP n 
  = executeBlock [Call "f" "fibP" [intToExp n]] [] [fibP] fibState

execFibM :: Int -> State
execFibM n 
  = executeBlock [Call "f" "fibM" [intToExp n]] [] [fibM, fibManager] fibState

