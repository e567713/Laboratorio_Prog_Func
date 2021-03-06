-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE GENERACION DE CODIGO DE MAQUINA


-- Se debe implementar la funcion generate, que dado
-- un AST que representa a un programa valido genera
-- el AST del codigo de maquina correspondiente

module Generator where


import Syntax
import MachineLang
-- se pueden agregar mas importaciones
-- en caso de ser necesario

generate :: Program -> Code
generate (Program name defs body) = generateBody body

generateBody :: Body -> Code
generateBody [] = []
generateBody (x:xs) = generateStmt x ++ generateBody xs


generateStmt :: Stmt -> Code
generateStmt (Assig name expr) = generateExpr expr ++ [STORE name]
generateStmt (Read name) = [READ] ++ [STORE name]
generateStmt (Write expr) = generateExpr(expr) ++ [WRITE]
generateStmt (If expr body1 body2) = generateExpr(expr)  ++ [JMPZ (length(generateBody(body1))+2)] ++ generateBody(body1) ++ [JUMP (length(generateBody(body2))+1)]++
           generateBody(body2) ++ [SKIP]
generateStmt(While expr body1) = generateExpr(expr) ++  [JMPZ (length(generateBody(body1))+2)] ++
        generateBody(body1) ++ [JUMP (-(length(generateBody(body1))+length(generateExpr(expr))+1))] ++ [SKIP]
generateStmt stmt = []

generateExpr :: Expr -> Code
generateExpr (IntLit int) = [PUSH int]
generateExpr (BoolLit bool) = if bool then [PUSH 1] else [PUSH 0]
generateExpr (Var name) = [LOAD name]
generateExpr (Unary uOp expr1)
  | uOp == Neg = generateExpr (expr1) ++ [NEG]
  | uOp == Not =  generateExpr(expr1) ++ [JMPZ 3] ++ [PUSH 0] ++ [JUMP 2] ++ [PUSH 1] ++ [SKIP]
generateExpr (Binary bOp expr1 expr2)
  | bOp == Plus = generateExpr (expr2) ++ generateExpr(expr1) ++ [ADD]
  | bOp == Equ = generateExpr (expr2) ++ generateExpr(expr1) ++ [CMP] ++[JMPZ 3] ++ [PUSH 0]++ [JUMP 2] ++ [PUSH 1] ++ [SKIP]
  | bOp == Less = generateExpr (expr2) ++ generateExpr(expr1) ++ [CMP] ++ [PUSH 1] ++ [ADD] ++ [JMPZ 3] ++ [PUSH 0] ++
        [JUMP 2] ++ [PUSH 1] ++ [SKIP]
  | bOp == Minus = generateExpr (expr2) ++ generateExpr(expr1) ++ [SUB]
  | bOp == Mult = generateExpr (expr2) ++ generateExpr(expr1) ++ [MUL]
  | bOp == Or = generateExpr (expr2) ++ generateExpr(expr1) ++ [JMPZ 3] ++ [JMPZ 1] ++ [PUSH 1] ++ [SKIP]
  | bOp == And = generateExpr (expr2) ++ generateExpr(expr1) ++ [JMPZ 2] ++ [JUMP 3] ++ [JMPZ 1] ++ [PUSH 0] ++ [SKIP]
  | bOp == Div = generateExpr (expr2) ++ generateExpr(expr1) ++ [DIV]
  | bOp == Mod = generateExpr (expr2) ++ generateExpr(expr1) ++ [MOD]
generateExpr _ = []