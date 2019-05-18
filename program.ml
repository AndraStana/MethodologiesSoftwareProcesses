open Data


(* --------------
a = 10
if(a)
{
    b = 2;
}
else
{
    a = a + b;
}
b = 10;
-------------- *)


let a =  CompStmt( AssignStmt ("a", Const 10),
            CompStmt(
                IfStmt (Var "a" ,
                    AssignStmt("b", Const 2),
                    AssignStmt("a", AddExp ( Var "a", Const 3))
                ),
                AssignStmt ("b", Const 10)
            )
);;

(* --------------
a = 8;
b = 1;
c = 0;
while(a){
    a = a - b;
    b = b + 1;
}
c = a + b;
-------------- *)

let b = CompStmt ( 
            AssignStmt ("a", Const 8),
            CompStmt (
                AssignStmt ("b", Const 1),
                CompStmt (
                    AssignStmt ("c", Const 0),
                    CompStmt (
                        WhileStmt ( Var "a", CompStmt (
                            AssignStmt ("a", DiffExp (Var "a", Var "b") ),
                            AssignStmt ("b", AddExp (Var "b", Const 1)) 
                            )), 
                        AssignStmt ("c", AddExp (Var("a"),Var("b") ))
                    )
                )
            )
);;

(* -----------------------------------Printing functions-------------------------------------- *)


let printInt number =  Printf.printf "%d" number;;
let printString text =  Printf.printf "%s" text;;

let rec printExp givenExp = match givenExp with 
    | Const (number) -> (printInt number)
    | Var (varName) -> (printString varName)
    | AddExp (exp1, exp2) -> (printExp exp1); Printf.printf " + "; (printExp exp2) 
    | DiffExp (exp1, exp2) -> (printExp exp1); Printf.printf " - "; (printExp exp2)
    | MulExp (exp1, exp2) -> (printExp exp1); Printf.printf " * "; (printExp exp2) 
    | DivExp (exp1, exp2) -> (printExp exp1); Printf.printf " / "; (printExp exp2) 



let rec printStmt givenStmt = match givenStmt with
    (* | _ ->  Printf.printf "%s " "stmt";; *)
    | CompStmt (stmt1, stmt2) -> (printStmt stmt1); (printStmt stmt2)
    | AssignStmt(varName, exp) -> (printString varName); Printf.printf " = "; (printExp exp)
    | PrintStmt (exp) -> Printf.printf "Print "; (printExp exp)
    | IfStmt (exp,stmt1,stmt2) -> Printf.printf " If( "; (printExp exp) ;  Printf.printf " ) { "; (printStmt stmt1);  Printf.printf "} else { "; (printStmt stmt1); Printf.printf " } "
    | WhileStmt(exp, stmt) -> Printf.printf "While ( "; (printExp exp); Printf.printf " ) {"; (printStmt stmt); Printf.printf " } " ;;
 
let rec printSuccessors successors = match successors with 
    | h :: t -> Printf.printf "%d " h; (printSuccessors t)
    | [] -> Printf.printf "" ;;
  
let printNode givenNode = match givenNode with 
        | (stmt, numberStmt, listSuccessors) -> Printf.printf "\n Statement: "; (printStmt stmt);
         Printf.printf "\n Statement Number: "; (printInt numberStmt);
         Printf.printf "\n Successors: "; (printSuccessors listSuccessors);;


Printf.printf "\n ************************************************************\n";;
printNode ( AssignStmt ("a", Const 2), 1, [2;3;4] );;

Printf.printf "\n ************************************************************\n";;
printNode ( a, 1, [2;3;4] )  ;;

(* -----------------------------------Control Flow Graph-------------------------------------- *)


let createControlFlowGraph program = match program with
        | CompStmt ( stmt1, stmt2 ) -> "CompStmt"
        (* | AssignStmt ( varName, exp) -> "AssignStmt" intra fara noduri succesoare *)
        | AssignStmt ( varName, exp) -> "AssignStmt" 
        
        | _ -> "DEFAULT";;

(* -----------------------------------Samples-------------------------------------- *)


Printf.printf "\n ************************************************************ \n";;
Printf.printf  " %s  \n" (createControlFlowGraph a);;





