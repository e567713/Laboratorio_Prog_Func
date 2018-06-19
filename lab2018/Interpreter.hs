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
                        let stack3 = pushStack stack2 (compareInt x y)
                        interp ((CMP):ant) xs (stack3,env)
interp ant ((JMPZ val):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let codes = movCode ant ((JMPZ val):xs) (val)
                        let ant1 = fst(codes)
                        let act1 = snd(codes)
                        if (x==0) then interp ant1 act1 (stack1,env) else interp ((JMPZ val):ant) xs (stack1,env)
interp ant ((JUMP val):xs) (stack, env) = do
                        let codes = movCode ant ((JUMP val):xs) val
                        let ant1 = fst(codes)
                        let act1 = snd(codes)
                        interp ant1 act1 (stack,env)
interp ant ((SKIP):xs) (stack, env) = do
                        interp ((SKIP):ant) xs (stack,env)
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
interp ant ((NEG):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let stack2 = pushStack stack1 (-x)
                        interp ((NEG):ant) xs  (stack2,env)
interp ant ((DIV):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let y = obtainTope stack1
                        let stack2 = popStack stack1
                        let stack3 = pushStack stack2 (div x y)
                        interp ((DIV):ant) xs  (stack3,env)
interp ant ((MOD):xs) (stack, env) = do
                        let x = obtainTope stack
                        let stack1 = popStack stack
                        let y = obtainTope stack1
                        let stack2 = popStack stack1
                        let stack3 = pushStack stack2 (mod x y)
                        interp ((MOD):ant) xs  (stack3,env)
interp _ [] conf = return conf
interp _ _ conf = return conf

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

compareInt :: Integer -> Integer -> Integer
compareInt value1 value2
    | value1 > value2 = 1
    | value1 == value2 = 0
    | value1 < value2 = -1

movCode :: Code -> Code -> Int -> (Code,Code)
movCode  [] [] _ = ([],[])
movCode  code1 code2 0 = (code1, code2)
movCode  (x:xs1) [] val
    | (val > 0) = ((x:xs1),[])
    | (val < 0) = movCode xs1 [x] (val+1)
movCode  [] (x:xs2) val
    | (val > 0) = movCode [x] xs2 (val-1)
    | (val < 0) = ([],(x:xs2))
movCode  (x:xs1) (y:xs2) val
    | (val > 0) = movCode (y:(x:xs1)) xs2 (val-1)
    | (val < 0) = movCode (xs1) (x:(y:xs2)) (val+1)



