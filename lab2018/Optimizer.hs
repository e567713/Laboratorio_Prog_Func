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
optimizeExpr expr = expr
    -- TODO hacer esto

alwaysTrue :: Expr -> Bool
alwaysTrue (BoolLit bool) = bool
alwaysTrue (Unary uOp expr)
  | uOp == Not = alwaysFalse expre
alwaysTrue (Binary bOp expr1 expr2)
  | bOp == Or = (alwaysTrue expr1) || (alwaysTrue expr2)
  | bOp == And = (alwaysTrue expr1) && (alwaysTrue expr2)
  | bOp == Equ = valorFinal expr == vlasof
  | bOp == Less =


alwaysFalse :: Expr -> Bool
alwaysFalse expr = True