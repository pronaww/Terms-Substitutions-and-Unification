let sig1 = [("Const0",0);("Const1",0);("Const2",0);("Const3",0);("Const4",0);("Const5",0);("Const6",0);("Const7",0);("Plus",2);("Times",2);("Minus",2);("Division",2);("F",1);("G",3);("H",4)];;
check_sig sig1;; (*true*)

let sig2 = [("c",1);("d",2);("e",3);("f",4);("g",5)];;
check_sig sig2;; (*false*)

let sig3 = [("c",0);("d",2);("e",3);("f",4);("f",5)];;
check_sig sig2;; (*false*)

let sig4 = [("c",0);("d",2);("e",3);("f",4);("g",-1)];;
check_sig sig2;; (*false*)

(*test cases for signature sig1*)
let a0 = Node(("Const0",0),[]);;
let a1 = V "y";;
let a2 = Node(("Const2",0),[]);;
let a3 = V("x");;
let a4 = Node(("Const4",0),[]);;
let a5 = V "z";;
let a6 = Node(("Const6",0),[]);;
let a7 = Node(("Const7",0),[]);;

wfterm a0;; (*true*)
wfterm a1;; (*true*)
wfterm a2;; (*true*)
wfterm a3;; (*true*)
wfterm a4;; (*true*)
wfterm a5;; (*true*)
wfterm a6;; (*true*)
wfterm a7;; (*true*)

ht a0;; (*0*) 
ht a1;; (*0*)
ht a2;; (*0*)
ht a3;; (*0*)
ht a4;; (*0*)
ht a5;; (*0*)
ht a6;; (*0*)
ht a7;; (*0*)

let b1 = Node(("Plus",2),[a0;a1]);;
let b2 = Node(("Times",2),[a1;a2]);;
let b3 = Node(("Minus",2),[a3;a3]);;
let b4 = Node(("Division",2),[a5;a6]);;
let b5 = Node(("F",1),[a5]);;
let b6 = Node(("G",3),[a6;a7;a4]);;
let b7 = Node(("H",4),[a1;a7;a1;a2]);;
let b8 = Node(("Plus",2),[a0;a3]);;

wfterm b1;; (*true*)
wfterm b2;; (*true*)
wfterm b3;; (*true*)
wfterm b4;; (*true*)
wfterm b5;; (*true*)
wfterm b6;; (*true*)
wfterm b7;; (*true*)

ht b1;; (*1*)
ht b2;; (*1*)
ht b3;; (*1*)
ht b4;; (*1*)
ht b5;; (*1*)
ht b6;; (*1*)
ht b7;; (*1*)

let c1 = Node(("Plus",2),[b5;b1]);;
let c2 = Node(("Times",2),[b1;b2]);;
let c3 = Node(("Minus",2),[b3;b4]);;
let c4 = Node(("Division",2),[b5;b6]);;
let c5 = Node(("F",1),[b5]);;
let c6 = Node(("G",3),[b6;b7;b3]);;
let c7 = Node(("H",4),[b1;b6;a1;a5]);;

wfterm c1;; (*true*)
wfterm c2;; (*true*)
wfterm c3;; (*true*)
wfterm c4;; (*true*)
wfterm c5;; (*true*)
wfterm c6;; (*true*)
wfterm c7;; (*true*)

ht c1;; (*2*)
ht c2;; (*2*)
ht c3;; (*2*)
ht c4;; (*2*)
ht c5;; (*2*)
ht c6;; (*2*)
ht c7;; (*2*)

let d1 = Node(("Plus",2),[c3;c1]);;
let d2 = Node(("Times",2),[c1;c2]);;
let d3 = Node(("Minus",2),[c3;c4]);;
let d4 = Node(("Division",2),[c5;c6]);;
let d5 = Node(("F",1),[c5;c6]);;
let d6 = Node(("G",3),[c6;c7]);;
let d7 = Node(("H",4),[c1;c6]);;

wfterm d1;; (*true*)
wfterm d2;; (*true*)
wfterm d3;; (*true*)
wfterm d4;; (*true*)
wfterm d5;; (*false*)
wfterm d6;; (*false*)
wfterm d7;; (*false*)

ht d1;; (*3*)
ht d2;; (*3*)
ht d3;; (*3*)
ht d4;; (*3*)

size d1;; (*14*)
size d2;; (*14*)
size d3;; (*15*)
size d4;; (*17*)

vars d1;; (*[V "x"; V "z"; V "y"]*)
vars d2;; (*[V "z"; V "y"]*)
vars d3;; (*[V "x"; V "z"]*)
vars d4;; (*[V "x"; V "z"; V "y"]*)

let s1 = [(V "x", (Node(("Const0",0),[])));(V "y", (Node(("Plus",2), [V "z"; (Node(("Const4",0),[]))])))];;
let s2=[(V "x",(Node(("f",1),[Node(("a",0),[])])));(V "y",(Node(("g",2),[(Node(("b",0),[]));V "z"])));(V "z",V "x")];;
let s3=[(V "x",(Node(("w",0),[])));(V "y",(Node(("h",1),[V "z"])));(V "z",(Node(("a",0),[])))];;
let s4 = [(V "x", (Node(("Const0",0),[]))); (V "y", (Node(("C",0),[])))];;

subst s1 d1;;
subst s1 d2;;
subst s1 d3;;
subst s1 d4;;

subst s2 d1;;
subst s2 d2;;
subst s2 d3;;
subst s2 d4;;

subst s3 d1;;
subst s3 d2;;
subst s3 d3;;
subst s3 d4;;

compose s1 s2;;
compose s2 s3;;
compose s3 s1;;

mgu d2 d1;; (* not unifiable *)
mgu b1 b8;; (* y -> x *)

mgu (subst s4 d1) d1;; (* substitution s4 *)

mgu d4 d4;; (* [] i.e. identity function *)
