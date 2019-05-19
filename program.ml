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


(* Printf.printf "\n ************************************************************\n";;
printNode ( AssignStmt ("a", Const 2), 1, [2;3;4] );;

Printf.printf "\n ************************************************************\n";;
printNode ( a, 1, [2;3;4] )  ;; *)

(* -----------------------------------Control Flow Graph-------------------------------------- *)


let createControlFlowGraph program = match program with
        | CompStmt ( stmt1, stmt2 ) -> "CompStmt"
        (* | AssignStmt ( varName, exp) -> "AssignStmt" intra fara noduri succesoare *)
        | AssignStmt ( varName, exp) -> "AssignStmt" 
        
        | _ -> "DEFAULT";;

(* -----------------------------------Samples-------------------------------------- *)


Printf.printf "\n ************************************************************ \n";;
(* Printf.printf  " %s  \n" (createControlFlowGraph a);; *)



(*****************************PRINTS****************************)
let rec printStringList lst = match lst with 
    | [] ->Printf.printf ""
    | h::t -> Printf.printf "%s " h; (printStringList t);;

let rec print_dict_list lst = match lst with
    | [] -> Printf.printf "\n"
    |h::t -> match h with 
          | (v,list) -> Printf.printf "Key: %d with values: " v ; (printStringList list); Printf.printf "\n" ; (print_dict_list t);;


(*************************************HELPERS******************** *)
(*TESTED.*)
let rec initDict n = match n with 
    | 0 -> []
    | _ -> List.append (initDict (n-1)) [(n,[])] ;;
(* TESTED.returns the value of in/out/def/use list of a number which correspondes to a node *)
let rec getDictValueByKey key dict = match dict with 
    | [] ->[]
    | (k, value)::t when k=key ->value
    | h::t -> (getDictValueByKey key t);;

(*TESTED.*)
let rec setDictValueByKey key dict newVal = match dict with
    | [] -> []
    | (k,_)::t when k=key -> List.append [(key,newVal)] ( setDictValueByKey key t newVal) 
    | h::t -> List.append [h] (setDictValueByKey key t newVal) ;;

(*TESTED.*)
let rec existsElementInList element list= match list with
    | [] -> false
    | h::t -> if h = element then true else (existsElementInList element t);;

(*TESTED.*)
let rec setDifference set1 set2 = match set1 with 
    | [] -> [] (*first one is the empty set -> always empty*)
    | h::t -> if (existsElementInList h set2) then (*if h is in set 2 we remove it*)
                    (setDifference t set2)
            else h::(setDifference t set2);;

(*TESTED.*)
let rec setUnion set1 set2 = match set2 with 
    | [] -> set1 (*second one is the empty set -> always the first*)
    | h::t -> if (existsElementInList h set1) then (*if h is in set 1 we skip it*)
                    (setUnion set1 t)
            else (setUnion (h::set1) t);;

(*TESTED. check for values of the same key to be equal -> set difference of values returns empty set*)
let rec dictAreEqual dict1 dict2 = match dict1 with 
    | [] -> true
    | (n,val1)::t-> let val2 = (getDictValueByKey n dict2) in
                let res= (setDifference val1 val2) in 
                    match res with 
                        | [] -> (dictAreEqual t dict2)
                        | _ -> false;;

(*TESTED*)
let rec listCount lst count= match lst with 
    | []-> count
    | _::t -> (listCount t (count+1));;

(*************************************ALGORITHM IMPL******************** *)

let rec getSuccessorsInput succ inN = match succ with 
    | [] -> []
    | h::t -> (setUnion (getSuccessorsInput t inN) (getDictValueByKey h inN));; 

(* *)
let forStep n succ inN outN inPrime outPrime use def = 
    let inPrime=(setDictValueByKey n inPrime (getDictValueByKey n inN)) and (* in'[n]=in[n]*)
    outPrime=(setDictValueByKey n outPrime (getDictValueByKey n outN)) and (* out'[n]=out[n] *)
    newIn = (setUnion (getDictValueByKey n use) (setDifference (getDictValueByKey n outN) (getDictValueByKey n def))) (*use[n] + (out[n]-def[n])*)
        and newOut = (getSuccessorsInput succ inN) in 
            let inN=(setDictValueByKey n inN newIn) and  outN=(setDictValueByKey n outN newOut) in
             (inN, outN, inPrime, outPrime);;

(*execute the for instructions one time for each node*)
let rec forLoop nodes inN outN inPrime outPrime use def = match nodes with
    | [] -> (inN, outN, inPrime, outPrime)
    | (_,n,succ)::t -> (*type node = stmt * int * listSuccessors *)
        let (newInN, newOutN, newInPrime, newOutPrime) = (forStep n succ inN outN inPrime outPrime use def ) in
            (forLoop t newInN newOutN newInPrime newOutPrime use def);;

(* *)
let rec repeatLoop nodes inN outN inPrime outPrime use def =  
    let (newIn,newOut,newPrimeIn,newPrimeOut)=(forLoop nodes inN outN inPrime outPrime use def) in (* excute for loop at least once *)
        if ((dictAreEqual newIn newPrimeIn) && (dictAreEqual newOut newPrimeOut)) then
            (newIn,newOut)
        else (repeatLoop nodes newIn newOut newPrimeIn newPrimeOut use def);;


(*************************************GET USE & DEF******************** *)
(*TESTED*)
let rec getAssignStatements stmt = match stmt with 
    | AssignStmt(varName, exp) -> [varName]
    | CompStmt (stmt1, stmt2) -> List.append (getAssignStatements stmt1) (getAssignStatements stmt2)
    | IfStmt (_,stmt1, stmt2) -> List.append (getAssignStatements stmt1) (getAssignStatements stmt2)
    | WhileStmt (_,stmt) -> (getAssignStatements stmt)
    | _ -> [];;

(*TESTED*)
let rec buildDef nodes def = match nodes with 
    | [] -> def
    | h::t -> match h with 
        | (stmt,n,_) -> let assignedVars = (getAssignStatements stmt) in 
            (buildDef t ( setDictValueByKey n def assignedVars));;

let rec getVarsFromExp exp = match exp with 
    | Var (varName) -> [varName]
    | AddExp (exp1, exp2) -> List.append (getVarsFromExp exp1) (getVarsFromExp exp2)
    | DiffExp (exp1, exp2) -> List.append (getVarsFromExp exp1) (getVarsFromExp exp2)
    | MulExp (exp1, exp2) -> List.append (getVarsFromExp exp1) (getVarsFromExp exp2)
    | DivExp (exp1, exp2) -> List.append (getVarsFromExp exp1) (getVarsFromExp exp2)
    | _ -> [];;

let rec getUsedVars stmt = match stmt with 
    | AssignStmt(_,exp) -> (getVarsFromExp exp)
    | PrintStmt (exp) -> (getVarsFromExp exp)
    | CompStmt (stmt1, stmt2) -> List.append (getUsedVars stmt1) (getUsedVars stmt2)
    | IfStmt (exp,stmt1,stmt2) -> List.append (getVarsFromExp exp) (List.append (getUsedVars stmt1) (getUsedVars stmt2))
    | WhileStmt (exp, stmt) -> List.append (getVarsFromExp exp) (getUsedVars stmt);;

(*TESTED*)
let rec buildUse nodes use = match nodes with 
    | [] -> use
    | h::t -> match h with 
        | (stmt,n,_) -> let usedVars = (getUsedVars stmt) in 
            (buildUse t ( setDictValueByKey n use usedVars));;

(*************MAIN************* *)
 let getLiveVars nodes = let n = (listCount nodes 0) in
    let inN = (initDict n) and outN = (initDict n) and inPrime = (initDict n) and outPrime = (initDict n) and use = (buildUse nodes (initDict n)) and def= (buildDef nodes (initDict n)) in
        (repeatLoop nodes inN outN inPrime outPrime use def);;


Printf.printf "\n *******************************INPUT & OUTPUT FILES***************************** \n";;

let dictList = [(1,["a";"b"]); (2,["c";"d"]) ];;
let lst= (initDict 3);;
(* (print_dict_list lst);; *)
let res1= (getDictValueByKey 2 dictList);;
(* (printStringList res1);; *)
let newDict =( setDictValueByKey 2 dictList ["e";"f"]);;
 (* (print_dict_list newDict);;  *)
let set1=["a";"b";"c"];;
let set2=["a";"b";"c"];;
(* (printStringList (setDifference set1 set2));; *)
(* (printStringList (setDifference set1 set2));; *)
(* let dictList2 = [(1,["a";"b"]); (3,["c";"d"]) ];;
Printf.printf "%b" (dictAreEqual dictList dictList2);; *)
(* Printf.printf "%d" (listCount set1 0);;  *)

let use =  [(1,[]); (2,["a"]); (3,["b";"c"]); (4, ["b"]); (5,["a"]); (6,["c"]) ];;
let def =  [(1,["a"]); (2,["b"]); (3,["c"]); (4, ["a"]); (5,[]); (6,[]) ];;


(* printNode ( AssignStmt ("a", Const 2), 1, [2;3;4] );; *)
let node1 =  ( AssignStmt ("a", Const 0), 1, [2] );;
let node2 =  ( AssignStmt ("b", AddExp(Var "a", Const 1) ), 2, [3] );;
let node3 =  ( AssignStmt ("c", AddExp(Var "c", Var "b") ), 3, [4] );;
let node4 =  ( AssignStmt ("a", MulExp(Var "b", Const 2) ), 4, [5] );;
let node5 =  ( PrintStmt (Var "a" ), 5, [2;6] );;
let node6 =  ( PrintStmt (Var "c" ), 6, [] );;
let nodes = [node1;node2;node3;node4;node5;node6];;

(*****************TEST USE & DEF***************** *)
let n =  (listCount nodes 0);;
let built_def = (buildDef nodes (initDict n));;
 Printf.printf "def: \n";;
(print_dict_list built_def);; 
 Printf.printf "use: \n";;
let built_use = (buildUse nodes (initDict n));;
(print_dict_list built_use);;

let inN= (initDict n);;
let outN= (initDict n);;
let inPrime =(initDict n);;
let outPrime = (initDict n);;

(*test for loop - exec once for every node*)
(* let (newIn,newOut,newPrimeIn,newPrimeOut)=(forLoop nodes inN outN inPrime outPrime built_use built_def);;
 Printf.printf "************* iteration1  \n";;
 Printf.printf "in: \n";;
(print_dict_list newIn);; 
 Printf.printf "inprime: \n";;
(print_dict_list newPrimeIn);;
 Printf.printf "out: \n";;
(print_dict_list newOut);; 
 Printf.printf "outprime: \n";;
(print_dict_list newPrimeOut);;
let (newIn,newOut,newPrimeIn,newPrimeOut)=(forLoop nodes newIn newOut newPrimeIn newPrimeOut use def);;
 Printf.printf "************* iteration2  \n";;
 Printf.printf "in: \n";;
(print_dict_list newIn);; 
 Printf.printf "inprime: \n";;
(print_dict_list newPrimeIn);;
 Printf.printf "out: \n";;
(print_dict_list newOut);; 
 Printf.printf "outprime: \n";;
(print_dict_list newPrimeOut);;
let (newIn,newOut,newPrimeIn,newPrimeOut)=(forLoop nodes newIn newOut newPrimeIn newPrimeOut use def);;
 Printf.printf "************* iteration3  \n";;
 Printf.printf "in: \n";;
(print_dict_list newIn);; 
 Printf.printf "inprime: \n";;
(print_dict_list newPrimeIn);;
 Printf.printf "out: \n";;
(print_dict_list newOut);; 
 Printf.printf "outprime: \n";;
(print_dict_list newPrimeOut);;
let (newIn,newOut,newPrimeIn,newPrimeOut)=(forLoop nodes newIn newOut newPrimeIn newPrimeOut use def);;
 Printf.printf "************* iteration4  \n";;
 Printf.printf "in: \n";;
(print_dict_list newIn);; 
 Printf.printf "inprime: \n";;
(print_dict_list newPrimeIn);;
 Printf.printf "out: \n";;
(print_dict_list newOut);; 
 Printf.printf "outprime: \n";;
(print_dict_list newPrimeOut);; *)

(*test repeat loop *)
 (* let (inN,outN) =  (repeatLoop nodes (initDict n) (initDict n) (initDict n) (initDict n) built_use built_def);; *)

let (inFinal, outFinal) = (getLiveVars nodes);;
 Printf.printf "Input for each node:\n";;
(print_dict_list inFinal);;
Printf.printf "Output for each node:\n";;
(print_dict_list outFinal);; 