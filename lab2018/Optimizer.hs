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
  | esValor (optimizeExpr expr) && esBool (optimizeExpr expr) = (BoolLit (not(obtainBool (optimizeExpr expr))))
  | esValor (optimizeExpr expr) = (IntLit (- obtainInt expr))
  | otherwise = (Unary uOp (optimizeExpr expr))
optimizeExpr (Binary bOp expr1 expr2)
  | bOp == Plus && (esValor opt1) && (esCero opt1) = opt2
  | bOp == Plus && (esValor opt2) && (esCero opt2) = opt1
  | bOp == Plus && (esValor opt1) && (esValor opt2) = (IntLit (num1 + num2))
  | bOp == Plus && (esValor opt1) && (esValor opt2) = (IntLit (num1 + num2))
  | bOp == Mult && (esValor opt1) && esCero (opt1) = (IntLit 0)
  | bOp == Mult && (esValor opt2) && esCero (opt2) = (IntLit 0)
  | bOp == Mult && (esValor opt1) && esUno (opt1) = opt2
  | bOp == Mult && (esValor opt2) && esUno (opt2) = opt1
  | bOp == Mult && (esValor opt1) && (esValor (opt2)) = (IntLit (num1 * num2))
  | bOp == Minus && (esValor opt2) && esCero (opt2) = opt1
  | bOp == Minus && (esValor opt1) && esCero opt1 = (IntLit (-obtainInt opt1))
  | bOp == And && (esValor opt1) && (esValor (opt2)) = (BoolLit (bool1 && bool2))
  | bOp == Less && (esValor opt1) && (esValor (opt2)) = (BoolLit (False))
  -- TODO hacer mod, div, or, equ
  | otherwise = (Binary bOp opt1 opt2)
  where
    num1 = obtainInt (optimizeExpr expr1)
    num2 = obtainInt (optimizeExpr expr2)
    bool1 = obtainBool (optimizeExpr expr1)
    bool2 = obtainBool (optimizeExpr expr2)
    opt1 = optimizeExpr expr1
    opt2 = optimizeExpr expr2

esValor :: Expr -> Bool
esValor (Var name) = False
esValor (IntLit int) = True
esValor (BoolLit bool) = True
esValor (Unary uOp expr) = False
esValor (Binary bOp expr1 expr2) = False

obtainInt :: Expr -> Integer
obtainInt (IntLit int) = int
obtainInt expr = -10

obtainBool :: Expr -> Bool
obtainBool (BoolLit bool) = bool
obtainBool expr = False


-- igualdad y menor solo se aplica a enteros
esBool :: Expr -> Bool
esBool (BoolLit bool) = True


esCero :: Expr -> Bool
esCero (IntLit int) = (int == 0)
esCero expr = False

esUno :: Expr -> Bool
esUno (IntLit int) = (int == 1)
esUno expr = False
--  | esValor (optimizeExpr expr) = (show (expr) == "1")
--  | otherwise = False


alwaysTrue :: Expr -> Bool
-- alwaysTrue expr = False
alwaysTrue (BoolLit bool) = bool
alwaysTrue (Unary uOp expr)
  | uOp == Not = alwaysFalse expr
alwaysTrue (Binary bOp expr1 expr2)
  | bOp == Or = (alwaysTrue expr1) || (alwaysTrue expr2)
  | bOp == And = (alwaysTrue expr1) && (alwaysTrue expr2)
  | bOp == Equ = False
  | bOp == Less && (esValor (optimizeExpr expr1)) && (esValor (optimizeExpr expr2)) =
    (obtainInt (optimizeExpr expr1)) < (obtainInt (optimizeExpr expr2))
  | otherwise = False


alwaysFalse :: Expr -> Bool
-- alwaysFalse expr = False
alwaysFalse (BoolLit bool) = not bool
alwaysFalse (Unary uOp expr)
  | uOp == Not = alwaysTrue expr
alwaysFalse (Binary bOp expr1 expr2)
  | bOp == Or = (alwaysFalse expr1) && (alwaysFalse expr2)
  | bOp == And = (alwaysFalse expr1) || (alwaysFalse expr2)
  | bOp == Equ && esValor(optimizeExpr expr1) && esValor(optimizeExpr expr2)= (optimizeExpr expr1) /= (optimizeExpr expr2)
  | bOp == Less && (esValor (optimizeExpr expr1)) && (esValor (optimizeExpr expr2)) =
    obtainInt (optimizeExpr expr1) >= obtainInt (optimizeExpr expr2)
  | otherwise = False



