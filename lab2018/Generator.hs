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
generateStmt (If expr body1 body2) = generateExpr(expr) ++ []
generateStmt stmt = []


generateExpr :: Expr -> Code
generateExpr (IntLit int) = [PUSH int]
generateExpr (BoolLit bool) = if bool then [PUSH 1] else [PUSH 0]
generateExpr (Var name) = [LOAD name]
generateExpr (Binary bOp expr1 expr2)
  | bOp == Plus = generateExpr (expr2) ++ generateExpr(expr1) ++ [ADD]
  | bOp == Equ = generateExpr (expr2) ++ generateExpr(expr1) ++ [CMP]
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