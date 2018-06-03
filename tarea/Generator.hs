-- LABORATORIO DE PROGRAMACION FUNCIONAL 2017
-- MODULO DE GENERACION DE CODIGO C

-- Se debe implementar la funcion genProgram que
-- dado un AST que representa un programa valido
-- genera el codigo C correspondiente


module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

-- CODE GENERATOR


genProgram :: Program -> String
genProgram (Program defs exprMain) = 
    "#include <stdio.h>\n" ++
    genDefs defs ++ 
    "int main() {\nprintf(\"%d\\n\"," ++
    --TAL VEZ HAY QUE CONSIDERAR QUE HAYAN LETS EN EL MAIN
    fst' (genExpr (exprMain,0,"")) ++
    "); }\n"

genDefs:: Defs -> String
genDefs [] = ""
genDefs (x:xs) = genFunDef x ++ genDefs xs

genFunDef:: FunDef -> String
genFunDef (FunDef (funName, (Sig parametersType returnType)) parametersName expr) = 
    "int _" ++ funName ++                           
    genParameters parametersName ++
    (trd' res) ++
    "return (" ++ 
    (fst' res) ++ 
    "); };\n"
    where res = (genExpr (expr, 0, ""))

genParameters:: [Name] -> String
genParameters names = 
    "(" ++ foldr (\x acc -> if null acc then "int _"++x++acc else "int _"++x++","++acc) "" names ++ "){\n"

genExpr:: (Expr,Int,String) -> (String,Int,String)
--Recibe:
--  Expr   Expresión a traducir a c.
--  Int    Número identificador de let actual.
--  String Lista de funciones lets declaradas.
--Devuelve:
--  String Expresión traducida a c.
--  Int    Número identificador de let actual. (Se devuelve para ser pasado nuevamente en la recursión)
--  String Lista de funciones lets declaradas.
genExpr ((Var nameVar),n,lets) = ('_':nameVar,n,lets)
genExpr ((IntLit int),n,lets) = (show int,n,lets)
genExpr ((BoolLit bool),n,lets) = if bool then ("1",n,lets) else ("0",n,lets)
genExpr ((Infix op expr1 expr2),n,lets) = ("(" ++ (fst' e1) ++ genOp op ++ (fst' e2) ++ ")",n,(trd' e2))
    where
        e1 = genExpr (expr1,n,lets)
        e2 = genExpr (expr2,snd' e1,trd' e1)
genExpr ((If expr1 expr2 expr3),n,lets) = ((fst' e1)++ "?" ++ (fst' e2) ++ ":" ++ (fst' e3),n,(trd' e3))
    where
        e1 = genExpr (expr1,n,lets)
        e2 = genExpr (expr2,snd' e1,trd' e1)
        e3 = genExpr (expr3,snd' e2,trd' e2)
genExpr ((Let (name, tipo) expr1 expr2),n,lets) = genLet ((Let (name, tipo) expr1 expr2), n, lets) 
genExpr ((App nameApp listaExprs),n,lets) = ('_':nameApp ++ "(" ++ (fst' l1) ++ ")",snd' l1,trd' l1)
    where
        l1 = genListExpr (listaExprs,n,lets)

genListExpr:: ([Expr],Int,String) -> (String,Int,String)
--Traduce a c una lista de expresiones
genListExpr ([],n,lets) = ([],n,lets)
genListExpr (x:xs,n,lets) = (if null xs then (fst' e1)++(fst' l1) else (fst' e1)++","++(fst' l1) ,snd' l1, trd' l1)
    where 
        e1 =  genExpr (x,n,lets)
        l1 = genListExpr (xs,snd' e1,trd' e1)
         
genLet::(Expr,Int,String) -> (String,Int,String)
--Devuelve:
--      String Expresión Let traducida incorporada a las expresiones ya traducidas.
--             Haré refencia al let que se está traduciendo como current let.
--      Int  Número identificador de lets procesados previo al current let + 1.
--      String Lista de funciones lets declaradas, con la current let incorporada.
genLet ((Let (name, tipo) expr1 expr2),processed_n,declared_lets) =
    
    (current_let_aplication, current_n+1, lets_declarations)
    where
        --Cuento lets en expr1 y expr2 y sumo a los que ya procesé.
        n1 = cantLetsIn expr1
        n2 = cantLetsIn expr2
        current_n = processed_n + n1 + n2

        --Proceso la expr1 (o hijo izquierdo) generando:
        --  fst: Expresión en c de la aplicación del current let.
        --  snd: Utilizo este valor para pasar el número de lets procesados antes de
        --       entrar en esta anidación (lets ya procesados antes que current let).
        --  trd: Declaraciones de los lets anidados en la expr1.
        --       Se colocan como procesos en c, antes que el proceso correspondiente a current let.
        pro_exp1 = genExpr (expr1,processed_n,[])

        --Genero la aplicación del let con la expresión obtenida del procesamiento de expr1.
        current_let_aplication = "_let" ++ show current_n ++ "("++ (fst' pro_exp1) ++ ")"

        --Proceso la expr2 (o hijo derecho) generando:
        --  fst: Expresión en c de del return del current let.
        --  snd: Utilizo este valor para pasar el número de lets procesados antes de
        --       entrar en esta anidación más la cantidad de lets procesados en expr1.
        --  trd: Declaraciones de los lets anidados en la expr2. 
        --       Se colocan como procesos en c, dentro de la declaración del proceso del current let.
        pro_exp2 = genExpr (expr2,processed_n + n1, [])
        
        --Genero la declaración del let con la expresión obtenida del procesamiento de expr1 y los lets hijos.
        current_let_declaration = "int _let" ++ show current_n ++ "(int _" ++ name ++ "){\n" ++ (trd' pro_exp2) ++ "return (" ++ (fst' pro_exp2) ++ "); };\n"

       --Agrego entonces a los lets ya declarados la declaración del current let y la de sus hijos.
        lets_declarations = (if null declared_lets then [] else declared_lets ) ++ --trd' pro_exp1 ++ "\n" ++ current_let_declaration
                            (if null (trd' pro_exp1) then [] else (trd' pro_exp1) ) ++ current_let_declaration

cantLetsIn:: Expr -> Int
cantLetsIn (Infix _ e1 e2) = cantLetsIn e1 + cantLetsIn e2
cantLetsIn (If e1 e2 e3) = cantLetsIn e1 + cantLetsIn e2 + cantLetsIn e3 
cantLetsIn (Let _ e1 e2) = 1 + cantLetsIn e1 + cantLetsIn e2
cantLetsIn (App _ list) = cantLetsInList list
cantLetsIn _ = 0

cantLetsInList:: [Expr] -> Int
cantLetsInList [] = 0
cantLetsInList (x:xs) = cantLetsIn x + cantLetsInList xs

genOp:: Op -> String
genOp op
    | op == Add = " + "
    | op == Sub = " - "
    | op == Mult = " * "
    | op == Div = " / "
    | op == Eq = "==" 
    | op == NEq = "!="
    | op == GTh = ">" 
    | op == LTh = "<"  
    | op == GEq = ">=" 
    | otherwise = "<="

--Funciones para trabajar con tuplas de 3 elementos.
fst' :: (a,b,c) -> a
fst' (x,y,z) = x

snd' ::  (a,b,c) -> b
snd' (x,y,z) = y

trd' :: (a,b,c) -> c
trd' (x,y,z) = z 