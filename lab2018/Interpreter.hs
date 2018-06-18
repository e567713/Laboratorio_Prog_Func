-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DEL INTERPRETE DEL LENGUAJE DE MAQUINA

-- Se debe implementar la funcion interp, que
-- dadas una lista con las instrucciones que estan
-- antes del program counter (PC), otra lista con la
-- instruccion en la que esta el PC y las posteriores,
-- y una configuracion de maquina inicial, retorna
-- la configuracion de maquina resultante de interpretar
-- el codigo. Esto se realiza en la monada IO porque
-- la interpretacion puede tener efectos de lectura y
-- escritura.

module Interpreter where

import MachineLang
-- se pueden agregar mas importaciones
-- en caso de ser necesario

-- configuracion de la maquina
type Conf = (Stack,Env)

-- ambiente de variables
type Env = [(Var,Integer)]
-- stack
type Stack = [Integer]

-- interprete
interp :: Code -> Code -> Conf -> IO Conf
interp ant ((READ):xs) (stack, env) = do
                        x <- getLine
                        interp ((READ):ant) xs ((pushStack stack (read x) ),env)
interp ant ((STORE name):xs) (stack, env) = do
                        let x = obtainTope stack
                        interp ((STORE name):ant) xs (popStack stack ,storeFunction env name x )
interp ant ((WRITE):xs) (stack, env) = do
                        putStrLn (show(obtainTope stack))
                        interp ((WRITE):ant) xs (popStack stack ,env)
interp ant ((PUSH value):xs) (stack, env) = do
                        interp ((PUSH value):ant) xs (pushStack stack value,env)
interp ant ((LOAD name):xs) (stack, env) = do
                        let x = searchFunction env name
                        interp ((LOAD name):ant) xs (pushStack stack (x),env)
interp ant ((CMP):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let y = obtainTope stack1
                        let stack2 = popStack stack1
                        if (x > y) then (let j = 1) else (if (x == y) then (let j = 0) else (let j = -1))

--                         if (x > y) then (let j = 1) else (if (x == y) then (let j = 0) else (let j = -1))
                        let stack3 = pushStack stack2 (j)
                        interp ((CMP):ant) xs (stack3,env)
interp ant ((ADD):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let y = obtainTope stack1
                        let stack2 = popStack stack1
                        let stack3 = pushStack stack2 (x+y)
                        interp ((ADD):ant) xs (stack3 ,env)
interp ant ((SUB):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack2 = popStack stack
                        let y = obtainTope stack2
                        let stack3 = popStack stack2
                        let stack4 = pushStack stack3 (x-y)
                        interp ((SUB):ant) xs (stack4 ,env)
interp ant ((MUL):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack2 = popStack stack
                        let y = obtainTope stack2
                        let stack3 = popStack stack2
                        let stack4 = pushStack stack3 (x*y)
                        interp ((MUL):ant) xs (stack4 ,env)
interp _ [] conf = return conf
interp _ _ conf = return conf

--Obtiene en integer de un Env
getInt :: Env -> Integer
getInt (x:xs) = getIntAux x
getInt [] = 11111111111111111111111

getIntAux :: (Var, Integer) -> Integer
getIntAux (var, int) = int


storeFunction :: Env -> Var -> Integer -> Env
storeFunction [] varObj value = [(varObj,value)]
storeFunction ((var,int):xs) varObj value
    | idemVar var varObj = (var,value):xs
    | otherwise = [(var,int)] ++ (storeFunction xs varObj value)


searchFunction :: Env -> String -> Integer
searchFunction [] varObj = 999999999
searchFunction ((var,int):xs) varObj
    | idemVar var varObj = int
    | otherwise = searchFunction xs varObj


idemVar :: Var -> Var -> Bool
idemVar (name1) (name2)
    | name1 == name2 = True
    | otherwise = False

obtainTope :: Stack -> Integer
obtainTope [] = 333333
obtainTope (x:xs) = x

popStack :: Stack -> Stack
popStack [] =[]
popStack (x:xs) = xs

pushStack :: Stack -> Integer -> Stack
pushStack xs val = val:xs


obpriName :: Env -> Var
obpriName [] = "VACIO"
obpriName ((name,int):xs) = name

-- compareInt :: Integer -> Integer -> Integer
-- compareInt value value




-- interp code1 code2 conf = do x <- getLine
--                                interp((READ):prev) xs (push (read x) s, e)



--
--
-- do x <- getLine
--     interp((READ):prev) xs (push (read x) s, e)
--
--
--
--  putStrLn (show i) -- Donde i es un Integer que sacaste del stack
--
--





