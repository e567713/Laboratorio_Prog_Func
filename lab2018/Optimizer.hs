-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE OPTIMIZACION

-- Se debe implementar la funcion optimize, que dado
-- un AST que representa a un programa valido genera
-- un AST de un programa equivalente optimizado
-- mediante la tecnica de constant folding como se
-- especifica en la letra

module Optimizer where

import Syntax
-- se pueden agregar mas importaciones
-- en caso de ser necesario



optimize :: Program -> Program
optimize (Program name defs []) = Program name defs []
optimize (Program name defs body) = Program name defs $ optimizeBody body


optimizeBody :: Body -> Body
optimizeBody [] = []
optimizeBody (x:xs) = optimizeStmt x ++ optimizeBody xs

optimizeStmt :: Stmt -> Body 
optimizeStmt (Assig name expr) = [(Assig name $ optimizeExpr expr)]
optimizeStmt (If expr body1 body2) 
    | alwaysTrue expr = cuerpo1
    | alwaysFalse expr = cuerpo2
    | otherwise = [(If  expresion cuerpo1  cuerpo2)]
    where
        cuerpo1 = optimizeBody body1
        cuerpo2 = optimizeBody body2
        expresion = optimizeExpr expr
optimizeStmt (While expr body)
    | alwaysFalse expr = []
    | otherwise = [(While expresion cuerpo)]
    where
        cuerpo = optimizeBody body
        expresion = optimizeExpr expr
optimizeStmt (Write expr) = [(Write  $ optimizeExpr expr)]
optimizeStmt (Read  name) = [(Read name)]



optimizeExpr :: Expr -> Expr
optimizeExpr (Var name) = (Var name)
optimizeExpr (IntLit int) = (IntLit int)
optimizeExpr (BoolLit bool) = (BoolLit bool)
optimizeExpr (Unary uOp expr) 
  | esValor (optimizeExpr expr) && esBool (optimizeExpr expr) = (BoolLit (not(read (show (optimizeExpr expr))::Bool)))
  | esValor (optimizeExpr expr) = (IntLit (-(read (show (optimizeExpr expr))::Integer)))
  | otherwise = (Unary uOp (optimizeExpr expr))
optimizeExpr (Binary bOp expr1 expr2)
  | bOp == Plus && esCero expr1 = expr2
  | bOp == Plus && esCero expr2 = expr1
  | bOp == Mult && esCero expr1 = (IntLit 0)
  | bOp == Mult && esCero expr2 = (IntLit 0)
  | bOp == Minus && esCero expr2 = expr1
  | bOp == Minus && esCero expr1 = (IntLit (-(read (show (optimizeExpr expr2))::Integer)))
  | otherwise = expr1


esBool :: Expr -> Bool
esBool expr = True


esCero :: Expr -> Bool
esCero expr = True

alwaysTrue :: Expr -> Bool
alwaysTrue (BoolLit bool) = bool
alwaysTrue (Unary uOp expr)
  | uOp == Not = alwaysFalse expr
alwaysTrue (Binary bOp expr1 expr2)
  | bOp == Or = (alwaysTrue expr1) || (alwaysTrue expr2)
  | bOp == And = (alwaysTrue expr1) && (alwaysTrue expr2)
  | bOp == Equ = (optimizeExpr expr1) == (optimizeExpr expr2)
  | bOp == Less && (esValor (optimizeExpr expr1)) && (esValor (optimizeExpr expr2)) = 
    (read (show (optimizeExpr expr1))::Int) < (read (show (optimizeExpr expr2))::Int)
  | otherwise = False 


alwaysFalse :: Expr -> Bool
alwaysFalse (BoolLit bool) = not bool
alwaysFalse (Unary uOp expr)
  | uOp == Not = alwaysTrue expr
alwaysFalse (Binary bOp expr1 expr2)
  | bOp == Or = (alwaysFalse expr1) && (alwaysFalse expr2)
  | bOp == And = (alwaysFalse expr1) || (alwaysFalse expr2)
  | bOp == Equ = (optimizeExpr expr1) /= (optimizeExpr expr2)
  | bOp == Less && (esValor (optimizeExpr expr1)) && (esValor (optimizeExpr expr2)) = 
    (read (show (optimizeExpr expr1))::Int) >= (read (show (optimizeExpr expr2))::Int)
  | otherwise = False 


esValor :: Expr -> Bool 
esValor (Var name) = False
esValor (IntLit int) = True 
esValor (BoolLit bool) = True
esValor (Unary uOp expr) = False 
esValor (Binary bOp expr1 expr2) = False 