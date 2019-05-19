open Data


(* --------------
a = 10
if(a)
{
    b = 2;
}
else
{
    a = a + 3;
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
a = 10
if(a)
{
    b = 2;
    c = 4;
}
else
{
    a = a + 3;
}
b = 10;
-------------- *)


let c =  CompStmt( AssignStmt ("a", Const 10),
            CompStmt(
                IfStmt (Var "a" ,
                    CompStmt (AssignStmt("b", Const 2), AssignStmt("c", Const 4) ),
                    AssignStmt("a", AddExp ( Var "a", Const 3))
                ),
                AssignStmt ("b", Const 10)
            )
);;

(* --------------
a = 10
if(a)
{
    b = 2;
    c = 4;
}
else
{
    a = a + 3;

    if( b ){
        c=7 
        }
        else
        {
            c=8
        }
    }
}
b = 10;
-------------- *)


let d =  CompStmt( AssignStmt ("a", Const 10),
            CompStmt(
                IfStmt (Var "a" ,
                    CompStmt (AssignStmt("b", Const 2), AssignStmt("c", Const 4) ),
                   CompStmt( AssignStmt("a", AddExp ( Var "a", Const 3)) , IfStmt(Var "b", AssignStmt ("c", Const 7),  AssignStmt ("c", Const 8) )    )
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



(* --------------
a = 8;

if( d )
{
    while(a){
        a = a - b;
        b = b + 1;
    }
    c = a + b;
}
else
{
    while(a){
        a = a - b;
    }
}
printf a;
-------------- *)

let e = CompStmt (
    AssignStmt( "a", Const 8),
    CompStmt(
        IfStmt(
            Var "d",
            CompStmt(
                        WhileStmt(Var "a",
                        CompStmt( AssignStmt ("a" , DiffExp( Var "a", Var "b")), AssignStmt ("b" , AddExp( Var "b", Var "1"))   )
                ),
                AssignStmt ("c" , AddExp( Var "a", Var "b"))

            ),

            WhileStmt( Var "a", AssignStmt( "a", DiffExp( Var "a", Var "b")))
        ),
        PrintStmt(Var "a")
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

let rec numberStatements program =  match program with
    | PrintStmt (exp) -> 1;
    | AssignStmt (varName, exp) -> 1;
    | CompStmt (stmt1, stmt2) -> (numberStatements stmt1) + (numberStatements stmt2)
    | IfStmt (exp, stmt1, stmt2) -> 1 + (numberStatements stmt1) + (numberStatements stmt2)
    | WhileStmt(exp, stmt) -> 1 + (numberStatements stmt);;

let rec generateListOfIds number = match number with
    | 0 -> []
    | _ -> number::(generateListOfIds (number - 1) )


let returnAndRemove list = match !list with
    | h::t -> list:= t; h
    | [] -> 0;;


let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2


 let rec createList program nodeList nodeId successorNodes listOfIds = match program with


        | PrintStmt (exp) -> if nodeId = 0 then  
                            let newList1  = ( ( PrintStmt ( exp ), (returnAndRemove  listOfIds) , successorNodes ) :: nodeList) in newList1
                            else 
                            let newList1  = ( ( PrintStmt ( exp ), nodeId , successorNodes ) :: nodeList) in newList1

        | AssignStmt (varName, exp) ->  if nodeId = 0 then  
                                    let newList1  = ( ( AssignStmt (varName, exp), (returnAndRemove  listOfIds), successorNodes ) :: nodeList)  in newList1 
                                    else
                                    let newList1  = ( ( AssignStmt (varName, exp), nodeId, successorNodes ) :: nodeList)  in newList1 

        | CompStmt ( stmt1, stmt2 ) -> if nodeId = 0 then
                                    let stmtId1 = (returnAndRemove  listOfIds) and  stmtId2 = (returnAndRemove  listOfIds)
                                    in (
                                        append (createList stmt1 nodeList stmtId1 [stmtId2] listOfIds) (createList stmt2 nodeList stmtId2 successorNodes listOfIds)
                                    )
                                 else
                                  let  stmtId2 = (returnAndRemove  listOfIds)
                                    in (
                                        append (createList stmt1 nodeList nodeId [stmtId2] listOfIds) (createList stmt2 nodeList stmtId2 successorNodes listOfIds)
                                    )
        | IfStmt (exp,stmt1,stmt2) -> 
                                        if nodeId = 0 then

                                        let stmtId1 = (returnAndRemove  listOfIds) and  stmtId2 = (returnAndRemove  listOfIds)
                                        in (
                                            let newList =  ( ( IfStmt (exp, stmt1, stmt2),(returnAndRemove  listOfIds) , [stmtId1; stmtId2] ) :: nodeList )
                                            in 
                                               ( append (append newList  (createList stmt1 nodeList stmtId1 successorNodes listOfIds) ) (createList stmt2 nodeList stmtId2 successorNodes listOfIds) 
                                            )
                                        )
                                        else
                                          let stmtId1 = (returnAndRemove  listOfIds) and  stmtId2 = (returnAndRemove  listOfIds)
                                        in (
                                            let newList =  ( ( IfStmt (exp, stmt1, stmt2),nodeId , [stmtId1; stmtId2] ) :: nodeList )
                                            in 
                                               ( append (append newList  (createList stmt1 nodeList stmtId1 successorNodes listOfIds) ) (createList stmt2 nodeList stmtId2 successorNodes listOfIds) 
                                            )
                                        )
        | WhileStmt (exp, stmt) ->   if nodeId = 0 then

                                        let whileId = (returnAndRemove  listOfIds) and  stmtId = (returnAndRemove  listOfIds)
                                        in (
                                            let newList =  (( WhileStmt (exp, stmt) , whileId, [stmtId] ) :: nodeList )
                                            in 
                                               ( append newList  (createList stmt nodeList stmtId  ( whileId :: successorNodes ) listOfIds) )
                                        )
                                        else
                                        
                                        let stmtId = (returnAndRemove  listOfIds)
                                        in (
                                            let newList =  (( WhileStmt (exp, stmt) , nodeId, [stmtId] ) :: nodeList )
                                            in 
                                               ( append newList  (createList stmt nodeList stmtId  (nodeId::successorNodes) listOfIds) )
                                        )
let createControlFlowGraph program =
 let ids = ref ( List.rev (generateListOfIds (numberStatements program))) in  createList program [] 0 [] ids;;

(* -----------------------------------Samples-------------------------------------- *)

Printf.printf "\n ************************************************************ \n";;

let test =  AssignStmt("b", Const 2);;
printControlFlowGraph (createControlFlowGraph test);;


let test2 =  CompStmt(  AssignStmt("a", Const 1),  CompStmt(  AssignStmt("b", Const 2), AssignStmt("c", Const 3) ) );;
printControlFlowGraph (createControlFlowGraph test2);;

printControlFlowGraph (createControlFlowGraph a);;
printControlFlowGraph (createControlFlowGraph c);;
printControlFlowGraph (createControlFlowGraph d);;
printControlFlowGraph (createControlFlowGraph b);;
printControlFlowGraph (createControlFlowGraph e);;













