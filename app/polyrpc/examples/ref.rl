
x : Ref {server} [String] = ref {server} [String] "one two three" ;

y : Unit = x := {server} [String] "four five six" ;

z : Unit = print {client} ( ! {server} [String] x )




