fun head (l : list int) = case l of
			      [] => -999
			    | x :: _ => x


val digitP = Otter.anyOfP (#"1" :: [])

val digit0P = Otter.anyOfP (#"0" :: #"1" :: [])

val unaryP = Otter.charP #"-"

(*Attempting to use this causes "Anonymous function remains at code generation" error

val numberP = Otter.choiceP (Otter.seqP (Otter.charP #"0") (Otter.notFollowedP (digitP)))
			    (Otter.seqP (Otter.seqP (Otter.optP unaryP) digitP) (Otter.repeatP digit0P))
*)

(*

This has not been ported yet

and numPI = num <- numberP ; return (read num)

and blankChP = anyOfP (#" " :: #"\t" :: [])

and blankP = Otter.repeatP blankChP

and blockPI = _ <- (Otter.seqP (Otter.charP #"(") blankP) ;
    val' <- expPI ;
    Otter.seqP (Otter.seqP blankP (charP #")") ) blankP ;
    return val'

and addDecPI = Otter.choiceP addPI decPI

and numBlkMulDivPI = Otter.choiceP (Otter.choiceP mulPI divPI) numBlkPI

and numBlkPI = Otter.choiceP blockPI numPI

and addPI =
    val1' <- numBlkMulDivPI ;
    Otter.seqP blankP (Otter.seqP (charP #"+") blankP) ; 
    val2' <- expPI ;
    return (head val1' + head val2' :: [])

and decPI =
    val1' <- numBlkMulDivPI ;
    Otter.seqP blankP (Otter.seqP (charP #"-") blankP) ; 
    val2' <- expPI ;
    return (head val1' + head val2' :: [])

and mulPI =
    val1' <- numBlkPI ;
    Otter.seqP blankP (Otter.seqP (charP #"*") blankP) ; 
    val2' <- numBlkMulDivPI ;
    return (head val1' * head val2' :: [])

and divPI =
    val1' <- numBlkPI ;
    Otter.seqP blankP (Otter.seqP (charP #"/") blankP) ; 
    val2' <- numBlkMulDivPI ;
    return ((head val1') / (head val2') :: [])

and expPI = _ <- blankP ;
    (Otter.choiceP addDecPI numBlkMulDivPI)
*)




fun f (s : string) = case Otter.runParse numberP (Otter.stringToList s) of
			 None => []
		       | Some (r, _) => r

fun main v =
  return <xml>
    <body>
      {[f v]}
    </body>
  </xml>

