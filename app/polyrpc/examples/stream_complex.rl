
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Streams                                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

data Stream = {l}. [a].  Nil | Cons a (Unit -l-> Stream {l} [a]) 

;

hd_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> a)
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        Cons x xs => x
      }

;

tl_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> Stream {l1} [a])
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        Cons x xs => xs ()
      }
      
;

map_stream
    : {l1 l2 l3 l4}. [a b]. ((a -l2-> b) -l3-> Stream {l1} [a] -l4-> Stream {l1} [b])
    = {l1 l2 l3 l4}. [a b].
      \f:a -l2->b @l3 xs:Stream {l1} [a] @l4 .
        case xs {
	 Nil => Nil {l1} [b];
         Cons y ys => Cons {l1} [b] (f y) (\unit : Unit @ l1 . map_stream {l1 l2 l3 l4} [a b] f (ys ()) )
	}

;

take_stream
    : {l1 l2 l3}. [a]. (Stream {l1} [a] -l2-> Int -l3-> Stream {l1} [a])
    = {l1 l2 l3}. [a].
      \s : Stream {l1} [a] @ l2
       n : Int @ l3 .
        case s {
	  Nil => Nil {l1} [a];
	  Cons y ys =>
	    if n <= 0
	    then Nil {l1} [a]
	    else Cons {l1} [a] y (\unit : Unit @ l1 . take_stream {l1 l2 l3} [a] (ys ()) (n-1))
	}
;

////////////////////////////////////////////////////////////////////////////////
// main
////////////////////////////////////////////////////////////////////////////////

client_list1 : Stream {client} [Int]
   = Cons {client} [Int] 1 (\unit:Unit @client.
      Cons {client} [Int] 2 (\unit:Unit @client.
        Cons {client} [Int] 3 (\unit:Unit @client. Nil {client} [Int])))
;

server_list1 : Stream {server} [Int]
   = Cons {server} [Int] 1 (\unit:Unit @server.
      Cons {server} [Int] 2 (\unit:Unit @server.
        Cons {server} [Int] 3 (\unit:Unit @server. Nil {server} [Int])))

;

test1 : Int
     = hd_stream {client client} [Int]
        (tl_stream {client client} [Int]
	  (take_stream {client client client} [Int]  
	    (map_stream {client client client client} [Int Int]
	       (\x:Int@client.x+1) client_list1)
	    2))
;

serverToclient
  : Stream {server} [Int] -client-> Stream {client} [Int]
  = \server_stream : Stream {server} [Int] @ client .
      case server_stream {
        Nil => Nil {client} [Int];
	Cons y ys =>
	  Cons {client} [Int] y
	    ( \unit:Unit@client. serverToclient (ys ()) )
      }
;

main : Stream {client} [Int] = serverToclient server_list1
