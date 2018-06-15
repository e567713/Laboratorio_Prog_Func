-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

-- Se debe implementar la funcion checkProgram, que
-- realiza los chequeos de nombres y tipos como se
-- especifica en la letra

module TypeChecker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- TYPE CHECKER

data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
 show (Duplicated      n)  = "Duplicated definition: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'




checkProgram :: Program -> [Error]
checkProgram (Program defs body) = etapa1 ++ etapa2 ++ etapa3
where
	etapa1 = checkNombresRepetidos defs
	etapa2 = checkNombresNoDeclarados (Program defs body)
	etapa3 = checkTipos(Program defs body)

checkNombresNoDeclarados :: Program -> [Error]
checkNombresNoDeclarados (Program defs body) = 
	usadosIncluidosEnDeclarados (nombresDeclarados defs, nombresUsados body)

usadosIncluidosEnDeclarados :: [Name] -> [Name] -> [Error]
--Se toma la lista xs como la lista de declarados
--Se toma la lista ys como la lista de usados
usadosIncluidosEnDeclarados xs [] = []
usadosIncluidosEnDeclarados xs ys =
	[Undefined y | y <- ys, notElem y xs]

nombresUsados :: Body -> [Name]
nombresUsados [] = []
nombresUsados (x:xs) = (getBodyName x) : nombresUsados xs

nombresDeclarados :: Defs -> [Name]
nombresDeclarados [] = []
nombresDeclarados (x:xs) = (getDefName x) ++ nombresDeclarados xs

getBodyName :: Stmt -> [Name]
getBodyName (Assig name expr) = [name] ++ getExprName expr  --no se si esta bien
															--no se que es Assig
getBodyName (If expr body1 body2) = getExprName expr ++ getBodyName body1 ++ getBodyName body2
getBodyName (While expr body) = getExprName expr ++ getBodyName body
getBodyName (Write expr) = getExprName expr
getBodyName (Read  name) = [name]

getExprName :: Expr -> [Name]
getExprName (Var name) = [name]
getExprName (IntLit int) = []
getExprName (BoolLit bool) = []
getExprName (Unary uOp expr) = getExprName expr
getExprName (Binary bOp expr1 expr2) = getExprName expr1 ++ getExprName expr2


getDefName :: VarDef -> [Name]
getDefName (VarDef (name, _)) = [name]


checkNombresRepetidos :: Defs -> [Error]
checkNombresRepetidos (defs) = 
	checkElementosRepetidos (nombresDeclarados defs)

checkElementosRepetidos :: [Name] -> [Error]
checkElementosRepetidos [] = []
checkElementosRepetidos [x] = []
checkElementosRepetidos (x:xs)
	| elem (last xs) (x:(init xs)) = (checkElementosRepetidos (x:(init xs))) ++ [Duplicated (last xs)]
	| otherwise = checkElementosRepetidos (x:(init xs))

--*****************************************
--De aca en adelante se hace el check de tipos
--*****************************************

checkTipos :: Program -> [Error]
checkTipos (Program defs []) = []
--*******************************
checkTipos (Program defs (x:xs)) = checkStmt x ++ checkTipos (Program defs xs)


--Chequea instrucciones if, while, readln, writeln
--checkStmt :: Stmt -> [Error]

--*******************************

--Devuelve lista de variables (Names) que deben ser booleanas
--Que son usadas en if, while
listaNamesQueDeberianSerTipoBool :: Stmt -> [Error]
--Assig Name Expr .... no se que es
listaNamesQueDeberianSerTipoBool (If expr body1 body2) = 
	esTipoBool expr ++
	listaNamesQueDeberianSerTipoBool body1 ++
	listaNamesQueDeberianSerTipoBool body2
listaNamesQueDeberianSerTipoBool (While expr body) =
	esTipoBool expr ++
	listaNamesQueDeberianSerTipoBool body 
listaNamesQueDeberianSerTipoBool (Write expr) = 


	getNameTipoBool expr ++ 
	listaNamesQueDeberianSerTipoBool body1 ++
	listaNamesQueDeberianSerTipoBool body2
listaNamesQueDeberianSerTipoBool (While expr body)

tipoCorrecto :: Body -> [Error]
tipoCorrecto [] = []
tipoCorrecto (x:xs) = 