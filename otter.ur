(* ported to ur from http://d.hatena.ne.jp/Otter_O/20090207/1233991312 *)

fun stringToList (s : string) : list char =
    let
	fun upTo acc i =
	    if eq i 0 then 0 :: acc
	    else upTo (i :: acc) (i - 1)
    in
	List.mp (fn i => strsub s i) (upTo [] (strlen s - 1))
    end

datatype parse i o = Parse of (i -> option (o * i))

fun runParse [i] [o] (p : parse i o) =
    case p of Parse f => f

val monad [i] = mkMonad { Return = fn [a] (o : a) =>
				      Parse (fn i => Some (o, i)),
			  Bind = fn [a] [b] (m : parse i a) (f : a -> parse i b) =>
				    Parse (fn s => case runParse m s of
						       None => None
						     | Some (o, s') => runParse (f o) s') }

fun _char [a] (f : a -> bool) (s : list a) : option (list a * list a) =
    case s of
	[] => None
      | c :: cs => if f c
		   then Some (c :: [], cs)
		   else None

fun charP [a] (e : eq a) (ch : a) = Parse (fn str => _char (eq ch) str)

fun anyOfP [a] (e : eq a) (chs : list a) =
    Parse (fn str => _char (fn i => List.exists (eq i) chs) str)

fun ifNothing [a] (n : option a) (x : a) =
    case n of
	None => Some x
      | Some _ => None

fun notFollowedP [i] [o] nf = Parse (fn i => ifNothing (runParse nf i) ([], i))

fun ifP [i] [o] (p : parse i o) (ps : o -> parse i o) (pf : parse i o) =
    Parse (fn s => case runParse p s of
		       None => runParse pf s
		     | Some (o, s') => runParse (ps o) s')

val emptyP [i] [o] : parse i (list o) = return []

fun optP [i] [o] (p : parse i (list o)) : parse i (list o) =
    ifP p return emptyP

fun repeatP [i] [o] (p : parse i (list o)) : parse i (list o) =
    ifP p (fn x =>
	      xs <- repeatP p ;
	      return (List.append x xs))
	emptyP

fun choiceP [i] [o] (p : parse i o) (q : parse i o) = ifP p return q

fun seqP [i] [o] (p : parse i (list o)) (q : parse i (list o)) =
    v <- p ; 
    w <- q ;
    return (List.append v w)
