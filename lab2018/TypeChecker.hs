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
checkProgram (Program name defs body)
  | not (null etapa1)  = etapa1
  | not (null etapa2)  = etapa2
  | not (null etapa3)  = etapa3
  | otherwise = []
 where
    etapa1 = checkNombresRepetidos defs
    etapa2 = checkNombresNoDeclarados (Program name defs body)
    etapa3 = checkTipos(Program name defs body)

-- **********************************************************************************
-- ****************  Check Etapa 1: Repetición de nombres  **************************
-- **********************************************************************************

getDefName :: VarDef -> [Name]
getDefName (VarDef name _) = [name]

nombresDeclarados :: Defs -> [Name]
nombresDeclarados [] = []
nombresDeclarados (x:xs) = (getDefName x) ++ nombresDeclarados xs

checkNombresRepetidos :: Defs -> [Error]
checkNombresRepetidos (defs) = checkElementosRepetidos (nombresDeclarados defs)

checkElementosRepetidos :: [Name] -> [Error]
checkElementosRepetidos [] = []
checkElementosRepetidos [x] = []
checkElementosRepetidos (x:xs)
    | elem (last xs) (x:(init xs)) = (checkElementosRepetidos (x:(init xs))) ++ [Duplicated (last xs)]
    | otherwise = checkElementosRepetidos (x:(init xs))

-- ****************************************************************************************************************

-- **********************************************************************************
-- ****************  Check Etapa 2: Nombres no declarados  **************************
-- **********************************************************************************

checkNombresNoDeclarados :: Program -> [Error]
checkNombresNoDeclarados (Program name defs body) = usadosIncluidosEnDeclarados nombresD nombresU
    where
    nombresD = nombresDeclarados defs
    nombresU = nombresUsados body

usadosIncluidosEnDeclarados :: [Name] -> [Name] -> [Error]
--Se toma la lista xs como la lista de declarados
--Se toma la lista ys como la lista de usados
usadosIncluidosEnDeclarados xs [] = []
usadosIncluidosEnDeclarados xs ys = [Undefined y | y <- ys, notElem y xs]

nombresUsados :: Body -> [Name]
nombresUsados [] = []
nombresUsados (x:xs) = (getStmtName x) ++ nombresUsados xs

getStmtName :: Stmt -> [Name]
getStmtName (Assig name expr) = [name] ++ getExprName expr
getStmtName (If expr body1 body2) = getExprName expr ++ nombresUsados body1 ++ nombresUsados body2
getStmtName (While expr body) = getExprName expr ++ nombresUsados body
getStmtName (Write expr) = getExprName expr
getStmtName (Read  name) = [name]

getExprName :: Expr -> [Name]
getExprName (Var name) = [name]
getExprName (IntLit int) = []
getExprName (BoolLit bool) = []
getExprName (Unary uOp expr) = getExprName expr
getExprName (Binary bOp expr1 expr2) = getExprName expr1 ++ getExprName expr2

-- ****************************************************************************************************************

-- **********************************************************************************
-- ****************  Check Etapa 3: Tipos  ******************************************
-- **********************************************************************************

checkTipos :: Program -> [Error]
checkTipos (Program name defs []) = []
-- *******************************
checkTipos (Program name defs body) = checkBody body defs

checkBody :: Body -> Defs -> [Error]
checkBody [] defs = []
checkBody (x:xs) defs = checkStmt x defs ++ checkBody xs defs


--Chequea instrucciones if, while, readln, writeln
checkStmt :: Stmt -> Defs-> [Error]
checkStmt (If expr body1 body2) defs 
  | tipoExpr expr defs == TyInt = checkExpr expr defs ++ [Expected TyBool TyInt] ++ checkBody body1 defs ++ checkBody body2 defs
  | tipoExpr expr defs == TyBool = checkExpr expr defs ++ [] ++ checkBody body1 defs ++ checkBody body2 defs
checkStmt (While expr body) defs 
  | tipoExpr expr defs == TyInt = checkExpr expr defs ++ [Expected TyBool TyInt] ++ checkBody body defs
  | tipoExpr expr defs == TyBool = checkExpr expr defs ++ [] ++ checkBody body defs 
checkStmt (Write expr) defs 
  | tipoExpr expr defs == TyInt = checkExpr expr defs ++ []
  | tipoExpr expr defs == TyBool = checkExpr expr defs ++ [Expected TyInt TyBool]
checkStmt (Read  name) defs  
  | tipoVariable name defs == TyInt = []
  | tipoVariable name defs == TyBool = [Expected TyInt TyBool]
checkStmt (Assig name expr) defs 
  | tipoVariable name defs == TyBool = if tipoExpr expr defs == TyBool then checkExpr expr defs ++ [] else checkExpr expr defs ++ [Expected TyBool TyInt]
  | tipoVariable name defs == TyInt = if tipoExpr expr defs == TyInt then checkExpr expr defs ++ [] else checkExpr expr defs ++ [Expected TyInt TyBool]


tipoExpr :: Expr -> Defs -> Type
tipoExpr (Var name) defs = tipoVariable name defs
tipoExpr (IntLit int) defs = TyInt
tipoExpr (BoolLit bool) defs = TyBool
tipoExpr (Unary uOp expr) defs
  | uOp == Not = TyBool
  | uOp == Neg = TyInt
tipoExpr (Binary bOp expr1 expr2) defs
  | bOp == Or = TyBool
  | bOp == And = TyBool
  | bOp == Equ = TyBool
  | bOp == Less = TyBool
  | bOp == Plus = TyInt
  | bOp == Minus = TyInt
  | bOp == Mult = TyInt
  | bOp == Div = TyInt
  | bOp == Mod = TyInt

--en las ultimas expresiones solo se fija el operador para saber el tipo
--el checkStm despues chequea la correctitud de expr


--data Expr =. Var     Name
--          | IntLit  Integer
--          | BoolLit Bool
--          | Unary   UOp Expr
--          | Binary  BOp Expr Expr


checkExpr :: Expr -> Defs -> [Error] --capas que hay que pasarle como parametro defs tambien
checkExpr (Var name ) defs = []
checkExpr (IntLit int) defs = []
checkExpr (BoolLit bool) defs = []
checkExpr (Unary uOp expr) defs
  | uOp == Not = if tipoExpr expr defs == TyBool then checkExpr expr defs ++ [] else checkExpr expr defs ++ [Expected TyBool TyInt]
  | uOp == Neg = if tipoExpr expr defs == TyInt then checkExpr expr defs ++ [] else checkExpr expr defs ++ [Expected TyInt TyBool]
checkExpr (Binary bOp expr1 expr2) defs 
  | bOp == Or = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkBool expr1 expr2 defs 
  | bOp == And = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkBool expr1 expr2 defs
  | bOp == Equ = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Less = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Plus = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Minus = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Mult = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Div = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs
  | bOp == Mod = checkExpr expr1 defs ++ checkExpr expr2 defs ++ checkInt expr1 expr2 defs


checkBool :: Expr -> Expr -> Defs -> [Error]
checkBool expr1 expr2 defs 
  | (tipoExpr expr1 defs == TyBool) && (tipoExpr expr2 defs == TyBool) = []
  | (tipoExpr expr1 defs == TyInt) && (tipoExpr expr2 defs == TyBool) = [Expected TyBool TyInt]
  | (tipoExpr expr1 defs == TyBool) && (tipoExpr expr2 defs == TyInt) = [Expected TyBool TyInt]
  | (tipoExpr expr1 defs == TyInt) && (tipoExpr expr2 defs == TyInt) = [Expected TyBool TyInt] ++ [Expected TyBool TyInt]


checkInt :: Expr -> Expr -> Defs -> [Error]
checkInt expr1 expr2 defs 
  | (tipoExpr expr1 defs == TyInt) && (tipoExpr expr2 defs == TyInt) = []
  | (tipoExpr expr1 defs == TyBool) && (tipoExpr expr2 defs == TyInt) = [Expected TyInt TyBool]
  | (tipoExpr expr1 defs == TyInt) && (tipoExpr expr2 defs == TyBool) = [Expected TyInt TyBool]
  | (tipoExpr expr1 defs == TyBool) && (tipoExpr expr2 defs == TyBool) = [Expected TyInt TyBool] ++ [Expected TyInt TyBool]


--nombresDeclarados []  []
--nombresDeclarados (x:xs)  (getDefName x) ++ nombresDeclarados xs

tipoVariable :: Name -> Defs -> Type
--tipoVariable name []  Null
tipoVariable name (x:xs)
  | name == getDefNameSolo x = getDefType x 
  | otherwise = tipoVariable name xs


getDefNameSolo :: VarDef -> Name
getDefNameSolo (VarDef name _) = name

getDefType :: VarDef -> Type
getDefType (VarDef _ ty) = ty


