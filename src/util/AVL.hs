-- | AVL tree implementation
-- |    https://www.cs.uleth.ca/~gaur/code/avl.hs


module AVL(find,ins,Tree(..)) where


data Tree a = Nil | Node !a !(Tree a) !(Tree a)
        deriving (Eq,Ord,Show,Read)

find :: Ord a => a -> Tree a -> Bool
find x Nil = False
find x (Node y l r)
  | x==y = True
  | x<y  = find x l
  | x>y  = find x r

height :: Tree a -> Integer
height Nil = 0
height (Node k l r) = 1 + (max (height l) (height r))

balanced :: Tree a -> Bool
balanced Nil = True
balanced  (Node k l r) | not (balanced l) = False
                       | not (balanced r) = False
                       | abs ((height l) - (height r)) > 1 = False
                       | otherwise = True

-- balanced (Node 3 (Node 2 (Node 1 Nil Nil) Nil) Nil)

left :: Tree a -> Tree a
left Nil = error ("left Nil")
left (Node n l r) = l

right :: Tree a -> Tree a
right Nil = error ("right Nil")
right (Node n l r) = r

value :: (Ord a) => Tree a -> a
value Nil = error ("value Nil")-- 0
value (Node n l r) = n


ins :: (Ord a) => Tree a -> a -> Tree a
ins Nil a = Node a Nil Nil
ins (Node b l r) k
  | b < k = rotate ((Node b l (ins r k)))
  | otherwise = rotate (Node b (ins l k) r)

rotate :: (Ord a) => Tree a -> Tree a
rotate Nil = Nil
rotate (Node n l r)
  | not (balanced l) = Node n (rotate l) r
  
  | not (balanced r) = Node n l (rotate r)
  
  | (height l) + 1 < (height r) &&    -- SR RR
    (height (left r))  < (height (right r)) =
       Node (value r) (Node n l (left r)) (right r)
       
  | (height r) + 1 < (height l) &&  -- SR LL
    (height (right l))  < (height (left l)) =
       Node (value l) (left l) (Node n (right l) r)
       
  | (height l) + 1 < (height r) && -- DR RL
    (height (left r))  > (height (right r)) = 
       Node (value (left r))
         (Node n l (left (left r)))
         (Node (value r) (right (left r)) (right r))
       
  | (height r) + 1 < (height l) && -- DR LR
    (height (right l))  > (height (left l)) =
       Node (value (right l))
         (Node (value l) (left l) (left (right l)))
         (Node n (right (right l)) r)
       
  | otherwise = Node n l r 

buildTree :: (Ord a) => [a] -> Tree a
buildTree [] = Nil
buildTree (x:xs) = foldl ins Nil (x:xs)

