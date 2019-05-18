open Data

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