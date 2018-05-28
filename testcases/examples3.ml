let sig1 = [("X",0);("Y",0);("f",1);("g",2);("h",3);("*",2)];;
let sig2 = [("X",0);("Y",0);("Z",0);("f",1);("g",2);("f",3);("*",2)];;
let sig3 = [("f",1)];;
let sig4 = [("X",0);("Y",0);("Z",0)];;

let term1 = (Node (("f",1),[V "X"]));;
let term2 = (Node (("g",2),[V "X";Node(("h",3),[Node(("f",1),[V "X"]);V "Y"])]));;
let term3 = (Node (("g",2),[V "X";Node(("*",2),[V "Y";Node (("*",2),[V "X";V "Y"])])]));;
let term4 = (Node (("g",2),[V "X";Node(("*",2),[V "Y";V "X"])]));;
let term5 = (Node (("g",2),[V "Z";Node(("*",2),[V "X";V "Z"])]));;
let term6 = (Node (("g",2),[V "Z";Node(("g",2),[V "X";V "Z"])]));;
let term7 = (V "X");;
let term8 = (Node (("K",0),[]));;
let term9 = (Node (("X",0),[]));;
let term10 = (Node (("g",2),[V "X";Node(("h",3),[Node(("f",1),[V "X"]);V "Y";Node (("X",0),[])])]));;
let term11 = (Node (("g",2),[V "X";Node(("h",3),[Node(("f",1),[V "X"]);V "Y";Node (("f",1),[V "X"])])]));;
let term12 = (Node (("g",2),[V "Z";Node(("*",2),[V "Z";Node (("*",2),[V "X";V "Y"])])]));;
let term13 = (Node (("$",2),[V "P";V"Q"]));;
let term14 = (Node (("$",2),[Node (("2",0),[]); Node (("4",0),[])]));;
let term15 = (Node (("$",2),[Node (("2",0),[]); Node (("3",0),[])]));;

Printf.printf "(1)check_sig sig1 : %B\n" (check_sig sig1);
Printf.printf "(2)check_sig sig2 : %B\n" (check_sig sig2);
Printf.printf "(3)check_sig sig3 : %B\n" (check_sig sig3);
Printf.printf "(4)check_sig sig4 : %B\n\n" (check_sig sig4);;

Printf.printf "(5)wfterm term1 sig1 : %B\n" (wfterm sig1 term1);
Printf.printf "(6)wfterm term2 sig1 : %B\n" (wfterm sig1 term2);
Printf.printf "(7)wfterm term7 sig4 : %B\n" (wfterm sig4 term7);
Printf.printf "(8)wfterm term8 sig4 : %B\n" (wfterm sig4 term8);
Printf.printf "(9)wfterm term9 sig4 : %B\n\n" (wfterm sig4 term9);;

Printf.printf "(10)ht term9 : %d\n" (ht term9);
Printf.printf "(11)ht term7 : %d\n" (ht term7);
Printf.printf "(12)ht term4 : %d\n" (ht term4);
Printf.printf "(13)ht term10 : %d\n" (ht term10);
Printf.printf "(14)ht term11 : %d\n\n" (ht term11);;

Printf.printf "(15)size term9 : %d\n" (size term9);
Printf.printf "(16)size term7 : %d\n" (size term7);
Printf.printf "(17)size term4 : %d\n" (size term4);
Printf.printf "(18)size term10 : %d\n" (size term10);
Printf.printf "(19)size term11 : %d\n\n" (size term11);;

vars term9;;
vars term7;;
vars term4;;
vars term10;;
vars term11;;

mgu term14 term13;;
mgu term3 term12;;
mgu term12 term3;;

subst (mgu term3 term12) term12;;
subst (mgu term3 term12) term3;;

subst (mgu term12 term3) term12;
subst (mgu term12 term3) term3;
