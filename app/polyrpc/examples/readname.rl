
name : String = read {client} ()

;

main : Unit = print {client}
                (concat {client}
		   (concat {client} "Hello " name)
		   " \n")