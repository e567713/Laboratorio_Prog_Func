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
optimize (Program name defs []) = Program name defs body
optimize (Program name defs (x:xs)) = Program name defs optimizeStmt x : optimizeBody xs


optimizeBody :: Body -> Body
optimizeBody name defs [] = Program name defs body 
optimizeBody name defs (x:xs) = Program name defs optimize x ++ optimizeBody xs

optimizeStmt :: Stmt -> Body 
optimizeStmt (Assig name expr) = [(Assig name optimizeExpr expr)]
optimizeStmt (If expr body1 body2) 
	| alwaysTrue expr = optimizeBody body1
	| alwaysFalse expr = optimizeBody body2
	| otherwise = [(If optimizeExpr expr optimizeBody body1 optimizeBody body2)]
optimizeStmt (While expr body)
	| alwaysFalse = []
	| otherwise = [(While optimizeExpr expr optimizeBody body)]
optimizeStmt (Write expr) = [(Write optimizeExpr expr)]
optimizeStmt (Read  name) = [(Read name)]