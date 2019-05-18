(* A program (Prg) in ToyWhile language consists of a statement (Stmt) as follows:
Prg := Stmt where the symbol "::=" means "a Prg is defined as a Stmt".
A statement can be either a compound statement (CompStmt), or an assignment statement
(AssignStmt), or a print statement (PrintStmt), or a conditional statement (IfStmt), or a while
statement (WhileStmt) as follows:
Stmt ::= Stmt1 ; Stmt2           /* (CompStmt)*/
 | Id = Exp                      /* (AssignStmt)*/
 | Print(Exp)                    /* (PrintStmt)*/
 | If Expr Then Stmt1 Else Stmt2 /* (IfStmt)*/
 | while Expr do Stmt            /*(WhileStmt) */
where the symbol "|" denotes the possible definition alternatives.
An expression (Exp) can be either an integer number (Const), or a variable name (Var), or an
arithmetic expression (ArithExp) as follows:
Exp ::= Number           /*(Const)*/
 | Id                    /*(Var)*/
 | Exp1 + Exp2          /*(ArithExp)*/
 | Exp1 - Exp2
 | Exp1 * Exp2
 | Exp1 / Exp2
where Number denotes an integer constant, and Id denotes a variable name. When an expression is
used as a condition in IfStmt and WhileStmt we use C-like convention, that means if expression
value is zero then that expression as condition is considered False otherwise if expression value is
not zero then that expression as condition is considered True.
*)

type progr = stmt

and 

stmt = CompStmt of stmt * stmt
    |  AssignStmt of string * exp
    | PrintStmt of exp
    | IfStmt of exp * stmt * stmt
    | WhileStmt of exp * stmt

and
 

 exp = Const of int  
    |   Var of string
    | AddExp of exp * exp
    | DiffExp of exp * exp
    | MulExp of exp * exp
    | DivExp of exp * exp













