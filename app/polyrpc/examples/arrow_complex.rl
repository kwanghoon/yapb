
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Arrow for function types                                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

// arrow[->]
arr_fun
  : {l}. [a]. (a -l-> a)
  = {l}. [a]. \x : a @ l . x  
;

// f >>> g
arr_seq_fun
  : {l1 l2 l3 l4 l5}. [a b c]. ( (a -l1-> b) -l3-> (b -l2-> c) -l4-> a -l5-> c )
  = {l1 l2 l3 l4 l5}. [a b c].
      \f : a -l1->b @ l3
       g : b -l2->c @ l4 
       x : a @ l5 .        g (f x)
;

// f &&& g
arr_par_fun 
  : {l1 l2 l3 l4 l5}. [a b c]. ( (a -l1-> b) -l3-> (a -l2-> c) -l4-> a -l5-> (b,c) )
  = {l1 l2 l3 l4 l5}. [a b c].
      \f : a -l1->b @ l3
       g : a -l2->c @ l4 
       x : a @ l5 . (f x, g x)
;

////////////////////////////////////////////////////////////////////////////////
// main program
////////////////////////////////////////////////////////////////////////////////

// arr (+)
arr_plus
  : {l}. ( (Int, Int) -l-> Int )
  = {l}. arr_fun {l} [(Int, Int) -l-> Int]
      (\xy : (Int,Int) @ l .
        case xy {
	  (x,y) => x + y
	}
      )
;

// f &&& g >>> arr (+)
addA
  : (Int -client-> Int) -client-> (Int -server-> Int) -client-> Int -client-> Int
  = \f : Int -client-> Int @ client 
     g : Int -server-> Int @ client  .
     arr_seq_fun {client client client client client} [Int (Int,Int) Int]
      (arr_par_fun {client server client client client} [Int Int Int] f g)
      (arr_plus {client})

;

main : Unit
  = print {client}
      (intToString {client} 
         (addA (\x : Int @ client . x+1) (\x : Int @ server . x-1) 10))
	 

