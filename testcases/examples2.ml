

let term = Node(("+",2), [V "x"; Node(("1",0), [])]);; (* x + 1 *)
let term2 = Node(("+",2), [V "x"; Node(("+",2), [Node(("1",0), [])])]);; (* invalid *)
let term3 = Node(("+",2), [Node(("+",2), [V "x"; V "y"]); Node(("-",2), [V "x"; V "y"])]);;
(* (x + y) + (x - y) *)
let term4 = V "z";;
let term5 = V "y";;

mgu term term3;;
(* Exception: NOT_UNIFIABLE *)
mgu term term4;;
(* Exception: NOT_UNIFIABLE. *)
mgu term3 term5;;
(* Exception: NOT_UNIFIABLE. *)


On 3 April 2018 at 15:13, shreshth tuli <shreshthtuli@gmail.com> wrote:

    let term = Node(Sym "+", [V "x"; Node(("1",0), [])]);; (* x + 1 *)
    let term2 = Node(Sym "+", [V (Var "x"); Node(Sym "+", [Node(Sym "1", [])])]);; (* invalid *)
    let term3 = Node(Sym "+", [Node(Sym "+", [V (Var "x"); V (Var "y")]); Node(Sym "-", [V (Var "x"); V (Var "y")])]);;
    (* (x + y) + (x - y) *)
    let term4 = V (Var "z");;
    let term5 = V (Var "y");;

    mgu term term3;;
    (* Exception: NOT_UNIFIABLE *)
    mgu term term4;;
    (* Exception: NOT_UNIFIABLE. *)
    mgu term3 term5;;
    (* Exception: NOT_UNIFIABLE. *)
