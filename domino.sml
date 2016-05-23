(*Siyuan Zhou
  Assignment 8
  Mar 3 2016
  *)

(*
given an integer, 
returns a list of tuples containing all the tiles in the set of double-N. *)
fun dominoes(N: int) =
    let
	    fun helper(max: int, min: int) =
	    (*base case for recursion return 0 if both 0, decretment min until 0, then decrease max*)
	        if max = 0 andalso min = 0 then [(0,0)]
	        else if max > 0 andalso min = 0 then (max, min)::helper(max-1, max-1)
	        else (max, min)::helper(max, min-1);
    in
    (*check if the N is 0 return empty list*)
	    if N < 1 then []
	   (*call helper function*)
	    else helper(N,N) 
    end    
    
(*given a list of tuples, returns a reordered list of tuples that forms one of possible circular trains.*)
(* Not quit work*) (*
fun Eulers(L:(int*int) list)=
	if null L then L
	else if length L = 1 then L
	else 
	let 
		fun find(l:(int*int) list)=
			if (length l = 1) orelse (#2(hd l) = #1 (hd(tl l))) then hd(tl l)
			else find(tl l)
	in
		  hd L::(find(L)::[])
	end
	*)
	(*This is just for solution and driver so you can run it*)
fun Eulers(L:(int*int) list)=
	L
	
(*Eulers([(2,2),(2,1),(2,0),(1,1),(1,0),(0,0)]);*)

(*given a list of tuples that form a circular train, return a list with tiles that are flipped
where appropriate to make the loop self-evident*)
fun flip(L:(int*int) list)=
(*check if it is an empty list and return empty list*) 
	if null L then []
	(*check if list has length 1 and return the list*)
	else if length L = 1 then L
	else
	let
	(*write swap function for swap pair in tuple*)
		fun swap(x:int,y:int)=
	  	(y,x)
	in
	(*check if first element in tuple equal the first element in next tuple then swap it*)
			if #1(hd L) = #1(hd(tl L))
		  then swap(hd L)::flip(tl L)
		  (*else iterate through the list*)
		  else hd L::flip(tl L)
	end	 
	
(*given an integer, generates a solution – ties all the functions given above in a functional manner*)
fun solution(N:int)=
(*check if N is odd*)
	if N mod 2 = 1 then []
	(*call all the functions above*)
	else flip(Eulers(dominoes(N)))
	
(*
given a list of tuples, returns a string representing that list
*)
fun listAsString(L:(int*int) list)=
(*check if the list null then return empty string*)
	if null L then " "
	(*iterate every element in list and convert int to string*)
	else "("^ Int.toString(#1 (hd L)) ^ "," ^Int.toString(#2 (hd L)) 
				^ ")" ^ listAsString(tl L) 
				
(*returns a string representing a solution to the program*)
fun driver(F1,F2) N=
	F1(F2(N)) 