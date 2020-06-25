
data List = [a]. Nil | Cons a (List [a])  ;

map : {l1 l2}. [a b]. ((a -l1-> b) -l2-> List [a] -l2-> List [b])
    = {l1 l2}. [a b].
      \f:a -l1->b @l2 xs:List [a] @l2 .
        case xs {
	 Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (map {l1 l2} [a b] f ys)
	};
	
	
main : List [Int] 
     = map {client server} [Int Int]
         (\x:Int @client . x + 5) (Cons [Int] 1 (Cons [Int] 2 (Cons [Int] 3 (Nil [Int]))))
	 