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
                        interp ((LOAD name):ant) xs (pushStack stack (searchFunction env name),env)
interp ant ((ADD):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack = popStack stack
                        let y = obtainTope stack
                        let stack = popStack stack
                        let stack = pushStack stack (x+y)
                        interp ((ADD):ant) xs (stack ,env)

interp _ [] conf = return conf
interp _ _ conf = return conf




storeFunction :: Env -> Var -> Integer -> Env
storeFunction ((var,int):xs) varObj value
    | idemVar var varObj = (var,value):xs
    | otherwise = [(var,int)] ++ (storeFunction xs varObj value)
storeFunction env _ _ = env

searchFunction :: Env -> Var -> Integer
searchFunction ((var,int):xs) varObj
    | idemVar var varObj = int
    | otherwise = searchFunction xs varObj
searchFunction _ _ = 77777

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





