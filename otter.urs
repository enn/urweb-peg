val stringToList : string -> list char

datatype parse i o = Parse of (i -> option (o * i))

val runParse : i ::: Type -> o ::: Type ->
	       parse i o -> i -> option (o * i)

val monad : i ::: Type -> monad (parse i)

val _char : a ::: Type ->
	    (a -> bool) -> list a -> option (list a * list a)
val charP : a ::: Type -> eq a ->
	    a -> parse (list a) (list a)
val anyOfP : a ::: Type -> eq a ->
	     list a -> parse (list a) (list a)
val notFollowedP : i ::: Type -> o ::: Type ->
		   parse i (list o) -> parse i (list o)

val ifP : i ::: Type -> o ::: Type ->
	  parse i o -> (o -> parse i o) -> parse i o -> parse i o

val emptyP : i ::: Type -> o ::: Type ->
	     parse i (list o)

val optP : i ::: Type -> o ::: Type ->
	   parse i (list o) -> parse i (list o)

val repeatP : i ::: Type -> o ::: Type ->
	      parse i (list o) -> parse i (list o)

val choiceP : i ::: Type -> o ::: Type ->
	      parse i o -> parse i o -> parse i o

val seqP : i ::: Type -> o ::: Type ->
	   parse i (list o) -> parse i (list o) -> parse i (list o)

