module SymbolTable where

-- eventually reach an AVL tree implementation of the symbol table
data Map key value = Leaf
                   | Branch key value Int (Map key value) (Map key value)
                   deriving Show

type SymbolTable = Map String Int

example :: SymbolTable
example = (Branch "a" 1 2 (Branch "b" 2 1 Leaf Leaf) (Branch "c" 3 1 Leaf Leaf))

exampleLeftHeavy :: SymbolTable
exampleLeftHeavy = (Branch "a" 1 2 (Branch "b" 2 1 (Branch "c" 3 1 Leaf Leaf) Leaf) Leaf)

exampleRightHeavy :: SymbolTable
exampleRightHeavy = (Branch "a" 1 2 Leaf (Branch "b" 3 1 Leaf (Branch "c" 2 1 Leaf Leaf)))


member :: SymbolTable -> String -> Bool
member Leaf _ = False
member (Branch ident _ _ l r) s
  | s == ident = True
  | s < ident  = member l s
  | otherwise  = member r s

-- insert or update
-- algorithm: check height before we do the recursive call, we call a recursive call
-- and then we do the rotate
insert :: SymbolTable -> String -> Int -> SymbolTable
insert Leaf k v = Branch k v 1 Leaf Leaf
insert (Branch ident val h l r) k v
  | ident == k = Branch k v (max (height l) (height r)) l r
  | k < ident  = let t = Branch ident val (h+1) (insert l k v) r in checkRotate t
  | otherwise  = let t = Branch ident val (h+1) l (insert r k v) in checkRotate t

checkRotate :: SymbolTable -> SymbolTable
checkRotate Leaf = Leaf
checkRotate (Branch k v h l r)
  | height l - height r > 1 = rotateLeft (Branch k v h l r)  -- change to rotateRight 
  | height r - height r > 1 = rotateLeft (Branch k v h l r)
  | otherwise               = Branch k v h l r


-- need to handle changing heights for all nodes below, probably done in checkRotate
rotateRight :: SymbolTable -> SymbolTable
rotateRight Leaf = Leaf
rotateRight (Branch k v h l r) = case l of
  Leaf -> error "Rotating right on a left leaf!"
  Branch k' v' h' l' r' -> case r of
    Leaf -> (Branch k' v' (h'+1) l' (Branch k v (h-1) r' Leaf))
    Branch kr vr hr lr rr -> (Branch k' v' (h'+1) l'
                                 (Branch k v (h-1) r' (Branch kr vr (hr-1) lr rr)) lr)

rotateLeft :: SymbolTable -> SymbolTable
rotateLeft Leaf = Leaf
rotateLeft (Branch k v h l r) = case l of
  Leaf -> case r of
    Leaf -> error "Both children are leaves, shouldn't have entered rotateLeft!"
    Branch k' v' h' l' r' -> (Branch k' v' (h'+1) (Branch k v (h-1) Leaf Leaf) r')
  Branch k' v' h' l' r' -> case r of
    Leaf -> error "Rotating left on right child being a leaf!"
    Branch kr vr hr lr rr -> (Branch kr vr (hr+1)
                               (Branch k v (h-1)
                                 (Branch k' v' (h'-1) l' r') lr) rr)

height :: SymbolTable -> Int
height Leaf = 0
height (Branch _ _ h _ _) = h
