-- LABORATORIO DE PROGRAMACION FUNCIONAL 2017
-- MODULO DE CHEQUEO

-- Se debe implementar la funcion checkProgram que
-- dado un AST que representa un programa
-- retorna una lista de mensajes de error en caso
-- de que el programa no sea correcto u Ok en otro caso.  


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type

data FunDesc = FunDesc {
                        name :: Name,
                        parametersType :: [Type],
                        numOfParameters :: Int
                       }
--Tipo creado para checkear cantidad y tipo de los argumentos en la APLICACIÓN de las funciones.
--La idea es llevar uan lista con la descripción de todas las funciones declaradas, y cada vez que me
--encuentro con la llamada (aplicación) de una función, busco en la lista de descripciones la
--descripción correspondiente a la función que encontré y checkeo el número de parámetros
--utilizando el valor numOfParameters y el tipo de los parámetros utilizando parametersType. 

instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)  = "The number of parameters in the definition of " ++ n ++
                             " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)  = "The number of arguments in the application of: " ++ n ++
                             " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


checkProgram :: Program -> Checked
checkProgram (Program defs body)
  | not (null etapa1)  = Wrong etapa1
  | not (null etapa2)  = Wrong etapa2
  | not (null etapa3)  = Wrong etapa3
  | not (null etapa4)  = Wrong etapa4
  | otherwise = Ok
  where
    etapa1 = checkFuncionesRepetidas defs ++ checkParametrosRepetidos defs
    etapa2 = checkNumeroDeParametros defs
    etapa3 = checkNombresNoDeclarados (Program defs body)
    etapa4 = checkTipos (Program defs body) [] (genAllFunctionsDesc defs)


-- ****************  Check Etapa 1: Repetición de nombres  ****************
checkFuncionesRepetidas :: Defs -> [Error]
checkFuncionesRepetidas [] = []
checkFuncionesRepetidas xs = checkRepetidos (getAllFunctionsNames xs)

checkParametrosRepetidos :: Defs -> [Error]
checkParametrosRepetidos [] = []
checkParametrosRepetidos (x:xs) = checkRepetidos (getDefParametersName x) ++ checkParametrosRepetidos xs

checkRepetidos:: [Name] -> [Error]
--Checkea la ocurrencia de elementos repetidos en una lista de nombres.
--Genera los errores de tipo Duplicated correspondientes.
checkRepetidos [] = []
checkRepetidos (x:xs)
  | elem x xs = [Duplicated x] ++ (checkRepetidos xs)
  | otherwise = checkRepetidos xs

getDefParametersName :: FunDef -> [Name]
--Devuelve una lista con los nombres de los parámetros de la definición de la función pasada por parámetro.
getDefParametersName (FunDef (_, _) paramNames _) = paramNames



-- ****************  Check Etapa 2: Número de parámetros  ****************
checkNumeroDeParametros :: Defs -> [Error]
checkNumeroDeParametros [] = []
checkNumeroDeParametros (x:xs)
  | defParams /= sigParams =  [ArgNumDef name sigParams defParams] ++ checkNumeroDeParametros xs
  | otherwise = checkNumeroDeParametros xs
  where
    defParams = getNumberOfParametersInDefinition x
    sigParams = getNumberOfParametersInSignature x
    name = getFunctionName x

getNumberOfParametersInDefinition :: FunDef -> Int
getNumberOfParametersInDefinition (FunDef (_, _) defParams _) = length defParams

getNumberOfParametersInSignature :: FunDef -> Int
getNumberOfParametersInSignature (FunDef (_, Sig sigParams _) _ _) = length sigParams



-- ****************  Check Etapa 3: Nombres no declarados  ****************
checkNombresNoDeclarados:: Program -> [Error]
checkNombresNoDeclarados (Program defs body) =
  checkNombresNoDeclaradosFunciones defs [] ++ checkNombresNoDeclaradosMain (Program defs body)

checkNombresNoDeclaradosFunciones :: Defs -> [Name] ->[Error]
checkNombresNoDeclaradosFunciones [] _ = []
checkNombresNoDeclaradosFunciones (x:xs) declaradosAntes = 
  checkNamesInExpr (getFunctionExpr x) declaredNames  ++ checkNombresNoDeclaradosFunciones xs (declaradosAntes ++ [getFunctionName x])
  where 
    declaredNames = declaradosAntes ++ [getFunctionName x] ++ getDefParametersName x

checkNombresNoDeclaradosMain :: Program -> [Error]
checkNombresNoDeclaradosMain (Program defs body) = checkNamesInExpr body declaredNames
  where 
    declaredNames = getAllFunctionsNames defs
    
checkNamesInExpr:: Expr -> [Name] -> [Error]
checkNamesInExpr (Var name) names = if elem name names then [] else [Undefined name]
checkNamesInExpr (IntLit  int) _ = []
checkNamesInExpr (BoolLit bool) _ = []
checkNamesInExpr (Infix   op expr1 expr2) names = checkNamesInExpr expr1 names ++ checkNamesInExpr expr2 names
checkNamesInExpr (If      expr1 expr2 expr3) names = checkNamesInExpr expr1 names ++ checkNamesInExpr expr2 names ++ checkNamesInExpr expr3 names
checkNamesInExpr (Let     (name,_) expr1 expr2) names = checkNamesInExpr expr1 names ++ checkNamesInExpr expr2 (name:names)
checkNamesInExpr (App     name listExprs) names = 
  (if elem name names then [] else [Undefined name]) ++ checkNamesInListOfExpr listExprs names
  
checkNamesInListOfExpr:: [Expr] -> [Name] -> [Error]
checkNamesInListOfExpr [] _ = []
checkNamesInListOfExpr (x:xs) names =
  checkNamesInExpr x names ++ checkNamesInListOfExpr xs names



-- ****************  Check Etapa 4: Chequeo de Tipos  ****************
checkTipos :: Program -> Env -> [FunDesc] ->[Error]
checkTipos (Program [] expr) env listDesc = checkExpr expr env listDesc
checkTipos (Program (x:xs) expr) env listDesc = 
  checkExpr e (env ++ fenv) listDesc ++
  (if (t /= returnType) then [Expected returnType t] else []) ++
  checkTipos (Program xs expr) (env ++ [(getFunctionName x, returnType)]) listDesc 
  where 
    fenv = genFunctionEnv x
    e = getFunctionExpr x
    t = typeExpr e (env ++ fenv)
    returnType = getFunctionType x

checkExpr :: Expr -> Env -> [FunDesc] ->[Error]
checkExpr (Var name) _ _ = []
checkExpr (IntLit  int) _ _ = []
checkExpr (BoolLit bool) _ _ = []
checkExpr (Infix op e1 e2) env listDesc
  | (op==Add || op==Sub || op==Mult || op==Div) = 
    (if ((te1==TyInt) && (te2==TyInt)) then []
      else if ((te1/=TyInt) && (te2/=TyInt)) then [Expected TyInt TyBool, Expected TyInt TyBool]
        else [Expected TyInt TyBool]) ++ continuarCheckeo
  | otherwise = 
    (if (te1==te2) then [] else [Expected te1 te2]) ++ continuarCheckeo
  where
    te1 = typeExpr e1 env 
    te2 = typeExpr e2 env 
    continuarCheckeo = checkExpr e1 env listDesc ++ checkExpr e2 env listDesc
checkExpr (If e1 e2 e3) env listDesc
  | ((te1==TyBool) && (te2==te3)) = [] ++ continuarCheckeo
  | ((te1==TyBool) && (te2/=te3)) = [Expected te2 te3] ++ continuarCheckeo
  | ((te1/=TyBool) && (te2==te3)) = [Expected TyBool TyInt] ++ continuarCheckeo
  | otherwise = [Expected TyBool TyInt, Expected te2 te3] ++ continuarCheckeo
  where
    te1 = typeExpr e1 env 
    te2 = typeExpr e2 env 
    te3 = typeExpr e3 env 
    continuarCheckeo = checkExpr e1 env listDesc++ checkExpr e2 env listDesc++ checkExpr e3 env listDesc
checkExpr (Let (name,tipo) e1 e2) env listDesc = 
  (if (tipo/=te1) then [Expected tipo te1] else []) ++ continuarCheckeo
  where 
     te1 = typeExpr e1 env
     continuarCheckeo = checkExpr e1 env listDesc ++ checkExpr e2 (extendEnv env (name,tipo)) listDesc
checkExpr (App name listExprs) env listDesc = 
  checkeoCantidadParametros ++ checkeoTipoParametros ++ checkeoExpresionesParametros
  where 
    fd = getFuncDesc name listDesc
    checkeoCantidadParametros = checkArgNumApp fd listExprs
    checkeoTipoParametros = checkArgTypeApp fd listExprs env
    checkeoExpresionesParametros = checkListExpr listExprs env listDesc

checkArgNumApp:: FunDesc->[Expr]->[Error]
--checkea que el numero de args en la aplicacion de la funcion sea correcto
checkArgNumApp fd listExprs = if a /= b then [ArgNumApp (name fd) a b] else []
  where
    a = numOfParameters fd
    b = length listExprs

checkArgTypeApp:: FunDesc->[Expr]->Env->[Error]
--checkea que el tipo de los args en la aplicacion de la funcion sea correcto
checkArgTypeApp (FunDesc _ [] _) listExprs _ = []
checkArgTypeApp (FunDesc _ t _) [] _ = []
--Si ya no me quedan args por checkear no devuelvo error (lo dice la letra)
checkArgTypeApp (FunDesc n (t:ts) c) (x:xs) env = 
  (if (t /= te) then [Expected t te] else []) ++ checkArgTypeApp (FunDesc n ts c) xs env
  where 
    te = typeExpr x env
  
checkListExpr:: [Expr] -> Env -> [FunDesc] -> [Error]
--checkea las expresiones de una lista de expresiones
checkListExpr [] _ _ = []
checkListExpr (x:xs) env listDesc = checkExpr x env listDesc ++ checkListExpr xs env listDesc

getFuncDesc:: Name -> [FunDesc] -> FunDesc
--obtiene la descripcion de la funcion de nombre n de la lista de descripciones global
getFuncDesc n (x:xs) = if name x == n then x else getFuncDesc n xs

typeExpr :: Expr -> Env -> Type
typeExpr (Var name) env = checkAmbiente name env
typeExpr (IntLit  int) env= TyInt
typeExpr (BoolLit bool) env= TyBool
typeExpr (Infix op e1 e2) env
  | (op==Add || op==Sub || op==Mult || op==Div) = TyInt
  | otherwise = TyBool 
typeExpr (If e1 e2 e3) env = typeExpr e2 env
typeExpr (Let typedVar e1 e2) env = typeExpr e2 (env ++ [typedVar])
typeExpr (App name listExprs) env = checkAmbiente name env

checkAmbiente:: Name -> Env -> Type
--checkea el tipo de una variable de nombre name en el ambiente env
checkAmbiente name env
  | elem (name,TyInt) env = TyInt
  | otherwise = TyBool

getAllFunctionsTypes:: Defs -> [Type]
--Devuelve una lista con los tipos de todas las funciones declaradas en el programa.
getAllFunctionsTypes [] = []
getAllFunctionsTypes ((FunDef (_, Sig _ returnType) _ _):xs) = returnType : (getAllFunctionsTypes xs)

genFunctionEnv:: FunDef -> Env
genFunctionEnv (FunDef (name, Sig parametersType retType) parametersName _) =  (name,retType):(zip parametersName parametersType)

genAllFunctionsDesc:: Defs -> [FunDesc]
--genera la descripcion de todas las funciones declaradas en el programa
genAllFunctionsDesc [] = []
genAllFunctionsDesc (x:xs) = 
  (FunDesc (getFunctionName x) (getFunctionParametersType x) (getNumberOfParametersInSignature x)) : (genAllFunctionsDesc xs)

getFunctionParametersType :: FunDef -> [Type]
getFunctionParametersType (FunDef (_, Sig sigParams _) _ _) = sigParams

extendEnv :: Env -> TypedVar -> Env
--Extiende el ambiente env con la variable pasada por parámetro.
--Sirve para las segundas expresiones de los lets,
--si encuentra una variable de igual nombre la "sobrescribe", sino se agrega al final.
extendEnv [] v = [v]
extendEnv ((n1,t1):xs) (n2,t2)
  | (n1 == n2) = (n2,t2):xs
  | otherwise = (n1,t1): extendEnv xs (n2,t2)


-- **************** Funciones utilizadas en más de una etapa *******************
--Etapa 1 , 2 y 4
getFunctionName :: FunDef -> Name
--Devuelve el nombre de la función pasada por parámetro.
getFunctionName (FunDef (name, _) _ _) = name

--Etapa 1, 3 y 4
getAllFunctionsNames:: Defs -> [Name]
--Devuelve una lista con los nombres de todas las funciones declaradas en el programa.
getAllFunctionsNames [] = []
getAllFunctionsNames (x:xs) = (getFunctionName x) : (getAllFunctionsNames xs)

--Etapa 3 y 4
getFunctionExpr :: FunDef -> Expr
getFunctionExpr (FunDef (_, _) _ expr) = expr

getFunctionType :: FunDef -> Type
getFunctionType (FunDef (_, Sig _ returnType) _ _) = returnType