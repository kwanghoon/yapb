
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Streams                                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

data Stream = {l}. [a].  SNil | SCons a (Unit -l-> Stream {l} [a]) 

;

hd_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> a)
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        SCons x xs => x
      }

;

tl_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> Stream {l1} [a])
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        SCons x xs => xs ()
      }
      
;

map_stream
    : {l1 l2 l3}. [a b]. ((a -l2-> b) -l3-> Stream {l1} [a] -l3-> Stream {l1} [b])
    = {l1 l2 l3}. [a b].
      \f:a -l2->b @l3 xs:Stream {l1} [a] @l3 .
        case xs {
	 SNil => SNil {l1} [b];
         SCons y ys => SCons {l1} [b] (f y) (\unit : Unit @ l1 . map_stream {l1 l2 l3} [a b] f (ys ()) )
	}


;

take_stream
    : {l1 l2}. [a]. (Stream {l1} [a] -l2-> Int -l2-> Stream {l1} [a])
    = {l1 l2}. [a].
      \s : Stream {l1} [a] @ l2
       n : Int @ l2 .
        case s {
	  SNil => SNil {l1} [a];
	  SCons y ys =>
	    if n <= 0
	    then SNil {l1} [a]
	    else SCons {l1} [a] y (\unit : Unit @ l1 . take_stream {l1 l2} [a] (ys ()) (n-1))
	}
;

////////////////////////////////////////////////////////////////////////////////
// main
////////////////////////////////////////////////////////////////////////////////

client_list1 : Stream {client} [Int]
   = SCons {client} [Int] 1 (\unit:Unit @client.
      SCons {client} [Int] 2 (\unit:Unit @client.
        SCons {client} [Int] 3 (\unit:Unit @client. SNil {client} [Int])))
;

server_list1 : Stream {server} [Int]
   = SCons {server} [Int] 1 (\unit:Unit @server.
      SCons {server} [Int] 2 (\unit:Unit @server.
        SCons {server} [Int] 3 (\unit:Unit @server. SNil {server} [Int])))

;

test1 : Int
     = hd_stream {client client} [Int]
        (tl_stream {client client} [Int]
	  (take_stream {client client} [Int]  
	    (map_stream {client client client} [Int Int]
	       (\x:Int@client.x+1) client_list1)
	    2))
	    
;

serverToclient
  : Stream {server} [Int] -client-> Stream {client} [Int]
  = \server_stream : Stream {server} [Int] @ client .
      case server_stream {
        SNil => SNil {client} [Int];
	SCons y ys =>
	  SCons {client} [Int] y
	    ( \unit:Unit@client. serverToclient (ys ()) )
      }
;

main : Stream {client} [Int] = serverToclient server_list1
