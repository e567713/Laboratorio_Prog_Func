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
generateStmt (If expr body1 body2) = generateExpr(expr)  ++ [JMPZ 3] ++ [PUSH 0] ++ [JUMP 2] ++ [PUSH 1] ++
           [SKIP] ++ [JMPZ (length(generateBody(body1))+2)] ++ generateBody(body1) ++ [JUMP (length(generateBody(body2))+1)]++
           generateBody(body2) ++ [SKIP]
generateStmt(While expr body1) = generateExpr(expr) ++ [JMPZ 3] ++ [PUSH 0] ++ [JUMP 2] ++ [PUSH 1] ++ [SKIP] ++ [JMPZ (length(generateBody(body1))+2)] ++
        generateBody(body1) ++ [JUMP (-(length(generateBody(body1))+length(generateExpr(expr))+6))] ++ [SKIP]
generateStmt stmt = []


generateExpr :: Expr -> Code
generateExpr (IntLit int) = [PUSH int]
generateExpr (BoolLit bool) = if bool then [PUSH 1] else [PUSH 0]
generateExpr (Var name) = [LOAD name]
generateExpr (Unary uOp expr1)
  | uOp == Neg = generateExpr (expr1) ++ [NEG]
generateExpr (Binary bOp expr1 expr2)
  | bOp == Plus = generateExpr (expr2) ++ generateExpr(expr1) ++ [ADD]
  | bOp == Equ = generateExpr (expr2) ++ generateExpr(expr1) ++ [CMP]
  | bOp == Less = generateExpr (expr2) ++ generateExpr(expr1) ++ [CMP] ++ [PUSH 1] ++ [ADD]
  | bOp == Minus = generateExpr (expr2) ++ generateExpr(expr1) ++ [SUB]
  | bOp == Mult = generateExpr (expr2) ++ generateExpr(expr1) ++ [MUL]
generateExpr _ = []



esValor :: Expr -> Bool
esValor (Var name) = False
esValor (IntLit int) = True
esValor (BoolLit bool) = True
esValor (Unary uOp expr) = False
esValor (Binary bOp expr1 expr2) = False

obtainBool :: Expr -> Bool
obtainBool (BoolLit bool) = bool
obtainBool expr = False