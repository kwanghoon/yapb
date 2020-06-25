
data Pair = [a]. Pair a a ;

id : {l}. [a]. (a -l-> a)
   = {l}. [a].
     \x : a @ l. x;

x : Int = id {client} [Int] 1;

main : Pair [Int] = id {client} [Pair [Int]] (Pair [Int] 1 2)
