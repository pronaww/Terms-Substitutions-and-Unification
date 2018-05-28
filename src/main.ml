open List;;

(* check_sig_arity checks if any symbol has arity less than zero *)
let rec check_sig_arity l = match l with
		[] -> true
	|	(s,ar)::xs -> if ar < 0 then false
						else check_sig_arity xs;;

(* check_sig_zero_arity checks atleast one zero arity symbol is present *)
let rec check_sig_zero_arity l = match l with
		[] -> false
	|	(s,ar)::xs -> if ar = 0 then true
						else check_sig_zero_arity xs;;

let table = Hashtbl.create 123456;;

(* filterOfTable checks if x is present if present then returns true else false *)
let filterOfTable x = if (Hashtbl.mem table x)=true then (true,()) else (false,(Hashtbl.add table x x));; 

(* check_sig_no_repeatition' tail recursive function for check_sig_no_repeatation*)
let rec check_sig_no_repeatition' l = match l with
		[] -> true
	|	(s,ar)::xs -> if (filterOfTable s)=(false,()) then check_sig_no_repeatition' xs
							else false;;

(* check_sig_no_repeatition checks no symbol is repeated *)
let check_sig_no_repeatition l = 
		Hashtbl.clear table;
		check_sig_no_repeatition' l;;

(* check_sig checks whether the signature is a valid signature(list of (symbol,arity) *)
let check_sig l = match l with
		[] -> false
	|  	_  -> if (check_sig_zero_arity l)&&(check_sig_arity l)&&(check_sig_no_repeatition l)&&((length l)>0) then true
				else false;;

(*----------------------------------------------------------------------------------------------------*)

type symbol = string*int;;
(* set of variables and symbols with arity *)
type term = V of string | Node of symbol*(term list);;

(* logical and function used in wfterm *)
let andfunc a b = if a= true &&b= true then true else false;;

let rec check a l = match l with
	[] -> false
	| x::xs -> if(x=a) then true else check a xs;;

let arity sym = match sym with
		(str, i) -> i;;

(* wfterm checks that a given preterm is well-formed or not according to the signature. *)
let rec wfterm sign t = match t with
		V(_) -> true
	| Node(sym, termList) -> if (check sym sign)&& ((arity sym)=(length termList)) then fold_left andfunc true (map (wfterm sign) termList)
												else false;;
												
(* max_number_list gives the maximum from the integer list l *)
let rec max_number_list l =
    match l with 
     [] -> 0
    |x::xs -> max x (max_number_list xs);;

(* ht gives return the height of a term given its well-formed *)
let rec ht t = match t with
		V(_) -> 0
	| Node((str,0),[]) -> 0
	| Node(sym, termList) -> 1 + max_number_list(map ht termList);;

(* simple addition function used in size *)
let sum a b = a + b;;

(* size gives return the no. of nodes in a term given its well-formed *)
let rec size t = match t with
		V(_) -> 1
	| Node(sym, termList) -> 1 + (fold_left sum 0 (map size termList));;

let varshash = Hashtbl.create 123456;;

let filter x = if (Hashtbl.mem varshash x)=true then (true,()) else (false,(Hashtbl.add varshash x x));; 

(* vars' gets all the variables in the term if uses filter to filter out the variables alrady appeared in the term and not make duplicates*)
let rec vars' l t = match t with
		V(str) -> if (filter (V(str)))=(false,()) then V(str)::l else l
	| Node(_, termList) -> (fold_left List.append [] (map (vars' []) termList))@l;;

(* vars gives the set of variables appearing in the well-formed term *)
let vars t = Hashtbl.clear varshash; vars' [] t;;

(*-------------------------------------------------------------------------------------------*)

(* find is function which takes list l of tuple ('a,'b) and element a and if element is present returns the corresponding 'b else return a itself *)
let rec find l a = match l with
		[] -> a
	| (x,y)::xs -> if x=a then y else find xs a;;

(* subst takes a term t and a substitution s as input, applies the (Unique Homomorphic Extension of) substitution s to t and returns the substituted term*)
let rec subst s t = match t with
		V(a) -> find s (V(a))
	| Node((str,0),[]) -> Node((str,0),[])
	| Node((str,n),l) -> Node((str,n),(map (subst s) l));;

(* checkPresent' returns false if element is not present in list else it returns the corresponding 'b from list l *)
let rec checkPresent' a l = match l with
	[] -> false
| (x,_)::xs -> if(x=a) then true else checkPresent' a xs;;

(* 
compose s1 s2 (x) = s1 o s2 if x belongs to dom(s2) \\this is implemented by compose2
					s1		if x belongs to dom(s1) and does not belongs to s2 \\this is implemented by compose1 *)

let rec compose2 s1 s2 l = match s2 with
		[] -> l
	| (x,y)::xs -> compose2 s1 xs ((x,subst s1 y)::l);;

let rec compose1 s1 s2 l = match s1 with
		[] -> l
	| (x,y)::xs -> if((checkPresent' x s2)= false) then (compose1 xs s2 ((x,y)::l)) else (compose1 xs s2 l);;

(* compose returns the substitution formed by composition of s1 and s2 *)
let compose s1 s2 = (compose1 s1 s2 [])@(compose2 s1 s2 []);;

exception NOT_UNIFIABLE;;

(* if t = f(t_1,...,t_k) and u = f(u_1,...,u_k) then fy applies the subst to get the composition of subst which is mgu here*)
let rec fy f l1 l2 s = match (l1,l2) with
	  (x1::xs1, x2::xs2) -> fy f xs1 xs2 (f (subst s x1) (subst s x2) s)
	| ([],[]) -> s;;

let rec reverse l1 l =match l with
[] -> l1
| x::xs -> reverse (x::l1) xs;;


(* mgu' takes terms t and u and list l = [] and gives the most general unifier l *)
let rec mgu' t u l = let boolXinL = checkPresent' t l and boolYinL = checkPresent' u l in
	match (t,u) with
	(V(x),V(y)) -> if x = y then l else if boolXinL=false && boolYinL=false then (V(x),V(y))::l
								   else if boolXinL=false && boolYinL=true then (V(x),V(y))::l
								   else if boolXinL=true && boolYinL=false then (V(y),V(x))::l
								   else raise NOT_UNIFIABLE
| (Node((str,0),[]),V(x)) -> if boolYinL=false then (V(x),Node((str,0),[]))::l
						else if (List.mem (V(x),Node((str,0),[])) l) then l
						else raise NOT_UNIFIABLE
| (V(x),Node((str,0),[])) -> if boolXinL=false then (V(x),Node((str,0),[]))::l
						else if (List.mem (V(x),Node((str,0),[])) l) then l
						else raise NOT_UNIFIABLE
| (V(x),Node((str,n),termList)) -> if (List.mem t (vars u))=false && (checkPresent' (V(x)) l)=false then (V(x),Node((str,n),termList))::l
							  else if (List.mem t (vars u))=false && (List.mem (V(x),Node((str,n),termList)) l)=true then l
							  else raise NOT_UNIFIABLE

| (Node((str,n),termList),V(x)) -> if (List.mem u (vars t))=false && (checkPresent' (V(x)) l)=false then (V(x),Node((str,n),termList))::l
							  else if (List.mem u (vars t))=false && (List.mem (V(x),Node((str,n),termList)) l)=true then l
							  else raise NOT_UNIFIABLE
| (Node((c1,0),[]),Node((c2,0),[])) -> if c1 = c2 then l else raise NOT_UNIFIABLE
| (Node((str,m),termList1),Node((g,k),termList2)) -> if str=g && m=k then (fy mgu' termList1 termList2 l) else raise NOT_UNIFIABLE;;

(* mgu gives most general unifier *)
let mgu t u = reverse [] (mgu' t u []);;
