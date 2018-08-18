module Language where

-- The abstract syntax of our single line language

type Output = [String]

data BinOp = Plus | Minus | Times | Div 
		   deriving Show

data Stm = CompoundStm Stm Stm
		 | AssignStm String Exp
		 | PrintStm [Exp]			-- should this be a non empty list data dec?
		 deriving Show

data Exp = IdExp String
		 | NumExp Int
		 | OpExp BinOp Exp Exp
		 | EseqExp Stm Exp
		 deriving Show

example :: Stm
example = CompoundStm (AssignStm "a" (OpExp Plus (NumExp 5) (NumExp 3)))
		    (CompoundStm (AssignStm "b" (EseqExp (PrintStm [IdExp "a", OpExp Minus (IdExp "a") (NumExp 1)]) (OpExp Times (NumExp 10) (IdExp "a")))) 
		       (PrintStm [IdExp "b"]))

-- first start off with counting the maximum number of arguments in a print stmt
maxArgs :: Stm -> Int
maxArgs (CompoundStm s1 s2) = max (maxArgs s1) (maxArgs s2)
maxArgs (PrintStm xs)		= length xs
maxArgs (AssignStm _ e)		= maxArgsInExp e

maxArgsInExp :: Exp -> Int
maxArgsInExp (OpExp _ e1 e2) = max (maxArgsInExp e1) (maxArgsInExp e2)
maxArgsInExp (EseqExp s e)   = max (maxArgs s) (maxArgsInExp e)
maxArgsInExp _ 				 = 0

-- then we write out interpreter, we only want to model behaviour
-- everything else is internal and we don't have to return that
-- why don't we just return an IO () for the sake of printing shiz to the user

interp :: Stm -> IO ()
interp stmt = do
  let (binds, out) = interpStmt stmt [] []
  let str = foldr (++) [] (reverse out)
  putStrLn str

interpStmt :: Stm -> [(String, Int)] -> Output -> ([(String, Int)], Output)
interpStmt (CompoundStm s1 s2) t o = let (t1, o1) = interpStmt s1 t o in
									   interpStmt s2 t1 o1
interpStmt (AssignStm v e) t o     = let (val, t1, o1) = interpExp e t o in
								       (update t1 v val, o1)
interpStmt (PrintStm []) t o       = (t,o)
interpStmt (PrintStm es) t o   	   =  go es t o
  where
  	go [] t o = (t,"\n":o)
        go [e] t o = let (val, t1, o1) = interpExp e t o in (t, "\n":show val:o)
  	go (e:es) t o = let (val, t1, o1) = interpExp e t o in go es t1 (" ":show val:o1)

interpExp :: Exp -> [(String, Int)] -> Output -> (Int, [(String, Int)], Output)
interpExp (NumExp n) t o = (n, t, o)
interpExp (IdExp v) t o  = case lookup v t of
	Just x  -> (x, t, o)
	Nothing -> error $ "Variable " ++ v ++ " has not been defined!"

interpExp (OpExp op e1 e2) t o = let (val1, t1, o1) = interpExp e1 t o in
                                   let (val2, t2, o2) = interpExp e2 t o in ((convertOp op) val1 val2, t2, o2)
  where
  	convertOp Plus  = (+)
  	convertOp Times = (*)
  	convertOp Div   = div
  	convertOp Minus = (-)
interpExp (EseqExp stm e) t o = let (t1, o1) = interpStmt stm t o in interpExp e t1 o1

-- given an environment, if the binding exists, update the value, else just tack on a new (key,value) pair
update :: [(String, Int)] -> String -> Int -> [(String, Int)]
update []     x val = [(x,val)]
update (k:ks) x val
  | (fst k) == x = (x, val) : ks 
  | otherwise    = k : update ks x val

