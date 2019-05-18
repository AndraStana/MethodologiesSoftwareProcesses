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
    (* | IfStmt (exp,stmt1,stmt2) -> Printf.printf " If( "; (printExp exp) ;  Printf.printf " ) { "; (printStmt stmt1);  Printf.printf "} else { "; (printStmt stmt1); Printf.printf " } "
    | WhileStmt(exp, stmt) -> Printf.printf "While ( "; (printExp exp); Printf.printf " ) {"; (printStmt stmt); Printf.printf " } " ;; *)
    | IfStmt (exp,stmt1,stmt2) -> Printf.printf " If( "; (printExp exp) ;  Printf.printf " ) "
    | WhileStmt(exp, stmt) -> Printf.printf "While ( "; (printExp exp); Printf.printf " ) ";;
 
let rec printSuccessors successors = match successors with 
    | h :: t -> Printf.printf "%d " h; (printSuccessors t)
    | [] -> Printf.printf "" ;;
  
let printNode givenNode = match givenNode with 
        | (stmt, numberStmt, listSuccessors) -> Printf.printf "\n Statement: "; (printStmt stmt);
         Printf.printf "\n Statement Number: "; (printInt numberStmt);
         Printf.printf "\n Successors: "; (printSuccessors listSuccessors);;

let rec printControlFlowGraph graph = match graph with 
    | h :: t ->  Printf.printf " \n \t \t Node: ";  printNode h; printControlFlowGraph t;
    | [] ->  Printf.printf "\n-----------------------------";;

 
(* -----------------------------------Control Flow Graph-------------------------------------- *)




       
        (* | _ ->  [];; *)




        (* | AssignStmt (varName, exp) -> let newList  = ( ( AssignStmt (varName, exp), 1, [] ) :: nodeList)  in  (let newList2 =  ( AssignStmt (varName, exp), 1, [] ):: newList in newList2) *)
       
       
        (* | AssignStmt (varName, exp) -> let newList  = ( ( AssignStmt (varName, exp), 1, [] ) :: nodeList)  in  (let newList2 =  ( AssignStmt (varName, exp), 1, [] ):: newList in newList2) *)
        (* | CompStmt ( stmt1, stmt2 ) -> ( stmt1, 1 , [] ):: nodeList; ( stmt2, 1 , [] ):: nodeList; nodeList *)

        (* | AssignStmt (varName, exp) -> ( AssignStmt (varName, exp), 1, [] ):: nodeList; nodeList
        | CompStmt ( stmt1, stmt2 ) -> ( stmt1, 1 , [] ):: nodeList; ( stmt2, 1 , [] ):: nodeList; nodeList *)

        (* | CompStmt ( stmt1, stmt2 ) -> ( stmt1, nodeNumber , [nodeNumber + 1] ):: nodeList; ( stmt2, nodeNumber +1 , [] ):: nodeList; nodeList *)

        


let rec numberStatements program number =  match program with
    | PrintStmt (exp) -> number + 1;
    | AssignStmt (varName, exp) -> number + 1;
    | CompStmt (stmt1, stmt2) -> number + (numberStatements stmt1 0) + (numberStatements stmt2 0)
    | IfStmt (exp, stmt1, stmt2) -> number + (numberStatements stmt1 0) + (numberStatements stmt2 0)
    | WhileStmt(exp, stmt) -> number + (numberStatements stmt 0);;

let rec generateListOfIds number = match number with
    | 0 -> []
    | _ -> number::(generateListOfIds (number - 1) )


        (* if number = 0 then (generateListOfIds (number - 1) number::list ) else list *)








 let rec createList program nodeList nodeNumber nodeNumberOfLoop = match program with 
       
        | PrintStmt (exp) -> let newList1  = ( ( PrintStmt ( exp), nodeNumber, [] ) :: nodeList) in newList1 
        | AssignStmt (varName, exp) -> let newList1  = ( ( AssignStmt (varName, exp), nodeNumber, [] ) :: nodeList)  in newList1
        
        
        
        (* ??????????? *)
        (* | IfStmt (exp,stmt1,stmt2) -> let newList1 = ( ( IfStmt (exp,stmt1,stmt2), nodeNumber, [nodeNumber + 1; nodeNumber + 2] ) :: nodeList)  in
                                        (
                                            let newList2 =  ( stmt1, nodeNumber + 1, [] ) :: newList1  in
                                            (
                                                let newList3 =  ( stmt2, nodeNumber + 2, [] ) :: newList2 in
                                                    newList3
                                            )
                                        ) *)




        (* | CompStmt ( stmt1, stmt2 ) -> let newList1  =  (stmt1, nodeNumber, [nodeNumber + 1] ) :: nodeList  in (let newList2 = (stmt2, nodeNumber + 1, [] ) :: newList1 in newList2) *)
        
        (* | CompStmt ( stmt1, stmt2 ) -> match stmt1, stmt2 with
         let newList1  =  (stmt1, nodeNumber, [nodeNumber + 1] ) :: nodeList  in (let newList2 = (stmt2, nodeNumber + 1, [] ) :: newList1 in newList2)
         *)

         (* | WhileStmt (exp, stmt) -> let newList =  (WhileStmt (exp, stmt), nodeNumber, []) :: nodeList in newList :: (createList, newList, nodeNumber + 1,nodeNumber) *)
        
        
        
        
        
        | _ -> [] ;; 
    
let createControlFlowGraph program = List.rev (createList program [] 1 0);;
    


(* 
let rec getLeastMaximumType program className1 className2 =
    let superClassesList1  = (getOrderedSuperClasses program className1)
        and superClassesList2  = (getOrderedSuperClasses program className2)
    in (getLeastMaximumTypeFromLists superClassesList1 superClassesList2);; *)


(* -----------------------------------Samples-------------------------------------- *)

(* printNode ( AssignStmt ("a", Const 2), 1, [2;3;4] );; *)

(* Printf.printf "\n ************************************************************\n";;
printNode ( a, 1, [2;3;4] )  ;; *)

Printf.printf "\n ************************************************************ \n";;
(* Printf.printf  " %s  \n" (createControlFlowGraph a);; *)

(* 
let test =  AssignStmt("b", Const 2);; 
printControlFlowGraph (createControlFlowGraph test);; *)


let test2 =  CompStmt( AssignStmt("a", Const 1), AssignStmt("b", Const 2) );; 
printControlFlowGraph (createControlFlowGraph test2);;

(* printSuccessors (generateListOfIds 10) *)
 





