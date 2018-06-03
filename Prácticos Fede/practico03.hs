import Data.List
-- PRÁCTICO 3 PROGRAMACIÓN FUNCIONAL 2017

--Recordatorio: Siempre que puedo y me doy cuenta utilizo pointless style.

-- ********************************** 1 ***************************************
-- 1. Explique el tipo de las siguientes funciones:
--       (a) min x y = if x < y then x else y
--       (b) paren x = "(" ++ show x ++ ")"

--(a)
--min:: (Ord a)->a->a->a
--min recibe dos parámetros y los compara, por lo tanto deben pertenecer a la clase Ord,
--lo que implica que tienen un orden establecido y pueden compararse.

--(b)
--paren:: (Show a)->a->String
--paren recibe un parámetro al que se le aplica la función show, por lo tanto debe pertenecer
--a la clase Show, lo que implica que el parámetro puede ser mostrado como un string.

-- ********************************** 2 ***************************************
--2. Dada la siguiente definición de tipo:
data Semaforo = Verde | Amarillo | Rojo
-- ¿Qué falta para que sea posible hacer (show Verde)?

--Falta derivar la función show o alternativamente hacerle override.

-- ********************************** 3 ***************************************
--3. Defina usando recursión explícita la función:
--   merge :: Ord a => [a]->[a]->[a]
--   que dadas dos listas ordenadas retorna una lista ordenada con el contenido
--   de ambas.
merge2:: Ord a => [a]->[a]->[a]
merge2 xs ys 
        | null xs = ys
        | null ys = xs
        | otherwise = if head_x < head_y then head_x:merge2 (tail xs) ys else head_y:merge2 xs (tail ys) 
        where
            head_x = head xs
            head_y = head ys



-- ********************************** 4 ***************************************
--4. Defina las siguientes funciones usando recursión explícita, como foldr y como foldl:

--(a) sumSqs:: Num a => [a] -> a, que suma los cuadrados de los elementos de una lista.
--(b) elem :: Eq a => a -> [a] -> Bool , que determina si un elemento pertenece a una lista.
--(c) elimDups :: Eq a => [a] -> [a], que elimina los duplicados adyacentes de una lista. 
--    Por ejemplo elimDups [1,2,2,3,4,4,4,3] retorna [1,2,3,4,3].

-- RECURSIÓN EXPLÍCITA
--Acá te uso pattern matching
sumSqs1:: Num a => [a] -> a
sumSqs1 [] = 0
sumSqs1 (x:[]) = x^2
sumSqs1 (x:xs) = x^2 + sumSqs1 xs

--Acá te hago magia con las guards
elem1:: Eq a => a -> [a] -> Bool
elem1 x xs
    | null xs = False
    | x == head xs = True
    | otherwise = elem1 x (tail xs)

elimDups1 :: Eq a => [a] -> [a]
elimDups1 [] = []
elimDups1 (x:[]) = [x]
elimDups1 (x:xs) = if x /= head xs then x : (elimDups1 xs) else elimDups1 xs 


-- RECURSIÓN CON foldr
--Usando lambdas
sumSqs2:: Num a => [a] -> a
sumSqs2 = foldr (\x acc -> x^2 + acc) 0

elem2:: Eq a => a -> [a] -> Bool
elem2 y = foldr (\x acc -> if x==y then True else acc) False 

elimDups2 :: Eq a => [a] -> [a]
elimDups2 [] = []
elimDups2 xs = foldr (\x acc -> if x /= head acc then x:acc else acc) [last xs] xs

-- RECURSIÓN CON foldl
--En esta no uso lambdas, uso where
sumSqs3:: Num a => [a] -> a
sumSqs3 = foldl res 0
        where res acc x = x^2 + acc

elem3:: Eq a => a -> [a] -> Bool
elem3 y = foldl (\acc x -> if x==y then True else acc) False 

elimDups3 :: Eq a => [a] -> [a]
elimDups3 [] = []
elimDups3 xs = foldl (\acc x -> if x /= last acc then acc ++ [x] else acc) [head xs] xs


-- ********************************** 5 ***************************************
--5. Sea h x xs = x - sum xs. ¿Cuál de las siguientes afirmaciones es correcta?
--   (a) h x xs = foldr (-) x xs
--   (b) h x xs = foldl (-) x xs
--(a) es falsa, ya que en la misma el acumulador es pasado como segundo parámetro
--de la función binaria (-), entonces al último elemento de xs se le está restando x,
--luego se le resta al penúltimo elemento de xs el resultado de la resta anterior y así sucesivamente
--y esto no es lo que hace h.

--(b) es correcta.

--Ejemplos:
-- si h x xs = x - sum xs      entonces h 10 [1,2,3] = 6
-- si h x xs = foldr (-) x xs  entonces h 10 [1,2,3] = -8
-- si h x xs = foldl (-) x xs  entonces h 10 [1,2,3] = 6


-- ********************************** 6 ***************************************
--6. Sea la función split :: [a] -> ([a],[a]), que divide la lista en dos listas
--colocando sus elementos de forma alternada. Por ejemplo split [2,4,6,8,7]
--resulta en ([2,6,7], [4,8]).

--(a) Implemente split como un foldr.
splitConFoldr::[a]->([a],[a])
splitConFoldr = foldr (\x (a,b) -> if (length a == length b) then (x:a,b) else (a,x:b)) ([],[]) 

--(b) Implemente split usando foldl.
splitConFoldl::[a]->([a],[a])
splitConFoldl = foldl (\(a,b) x -> if length a == length b then (a ++ [x],b) else (a,b ++ [x])) ([],[])


-- ********************************** 7 ***************************************
--7. Sea la función maxInd :: Ord a => [a] -> (a,Int), que retorna el máximo
--   de una lista no vacía y el índice de su primera ocurrencia. Los índices se
--   comienzan a numerar en 0. Por ejemplo maxInd [8,10,6,10,10] resulta
--   en (10,1).

--(a) Implemente maxInd usando foldl.
maxIndConFoldl :: Ord a => [a] -> (a,Int)
maxIndConFoldl xs
            | null xs = error "Lista vacia no titan"
            | otherwise = foldl (\(m,ind) x -> if x > m then  (x, (\ (Just i)->i)(elemIndex x xs)) else (m,ind)) (head xs,0) xs
--Demasiado tryhard esto, seguramente había una mejor forma.
--Usé la función elemIndex, la cual dado un elemento y una lista, retorna el indice de la
--primera ocurrencia de ese elemento. (Pertenece a la libreria Data.list)
--No leí mucho, pero parece que es función devuelve algo de tipo Maybe Int, una chota.
--Entonces hice un lambda que transforme algo de ese tipo a un Int


--(b) Implemente maxInd usando foldr.
maxIndConFoldr :: Ord a => [a] -> (a,Int)
maxIndConFoldr xs
            | null xs = error "Lista vacia no titan"
            | otherwise = foldr (\x (m,ind)-> if x >= m then  (x, (\ (Just i)->i)(elemIndex x xs)) else (m,ind)) acc xs
            where
                acc = (maximo, indiceMaximo)
                maximo = last xs
                indiceMaximo = length xs - 1
--Estupidamente ineficiente 
--Quería hacerme el inteligente con ese comentario.

-- ********************************** 8 ***************************************
--8. Implemente las funciones takeWhile y dropWhile usando:

--(a) foldr.
takeWhileConFoldr:: (a->Bool)->[a]->[a]
takeWhileConFoldr p = foldr (\x acc -> if p x then x:acc else []) []

dropWhileConFoldr:: (a->Bool)->[a]->[a]
dropWhileConFoldr p xs = foldr (\x acc -> if (p x) && (p (head acc)) then tail acc else acc) xs (reverse xs)
--Que lo parió que me costó esta criatura, por eso la explico:
--La idea es revertir la lista para recorrerla desde el "inicio", y dropear elementos
--(con el tail) hasta encontrar un elemento que no cumpla el predicado.
--Me ocurría que dropeaba bien los elementos que no cumplían al inicio, luego no 
--dropeaba el que fallaba, pero volvía a dropear cuando los elementos cumplían,
--digamos que no sabía como detener la función al encontrar el primer false.
--La clave es que una vez que se encuentre el primer elemento que no cumple,
--(p (head acc)) será siempre false, ya que en la cabeza de la sol queda 
--el elemento que dio false y no lo saco, entonces no dropeo más elementos de la solución.
--Recordar que en acc llevo la lista solución, que comienza siendo igual a la lista 
--pasada por parámetro.


--(b) foldl.
takeWhileConFoldl:: (a->Bool)->[a]->[a]
takeWhileConFoldl p xs = foldl (\acc x -> if p x then x:acc else []) [] (reverse xs)

dropWhileConFoldl:: (a->Bool)->[a]->[a]
dropWhileConFoldl p = foldl (\acc x -> if p x && null acc then [] else acc++[x]) []
--Bueno otra variante de como hacer esa que estaba dificil en mi opinión.
--Comienzo con acc vacío, y una vez que el predicado es false, agrego al acc,
--luego agrego siempre pues pregunto si el acc es vacío o tiene algún elemento.


-- ********************************** 9 ***************************************
--9. Suponga que representamos números naturales como listas de dígitos ordenados 
--   de forma descendente según su significación. Por ejemplo [1,2,5] representa al número 125.

-- (a) Defina una función decimal :: [Int] -> Int, que dado un natural en
-- esta representación compute el entero correspondiente. Escriba dicha
-- función utilizando recursión explícita y como foldl.

--RECURSIÓN EXPLÍCITA
decimalRecExp1:: [Int]->Int
decimalRecExp1 xs
                | null xs = error "parametro incorrecto"
                | length xs == 1 = head xs
                | otherwise = head xs * (10^(length xs - 1)) + decimalRecExp1 (tail xs)
--Es solo descomponer el numero en base 10

--Otra forma
decimalRecExp2:: [Int]->Int
decimalRecExp2 [] = error "parametro incorrecto"
decimalRecExp2 (x:[]) = x
decimalRecExp2 (x1:x2:xs) = decimalRecExp2(read (show x1 ++ show x2):xs)

--Otra má
decimalRecExp3:: [Int]->Int
decimalRecExp3 [] = error "parametro incorrecto"
decimalRecExp3 (x:[]) = x
decimalRecExp3 (x1:x2:xs) = decimalRecExp2((x1*10+x2):xs)

--COMO foldl
decimalConFoldl:: [Int]->Int
decimalConFoldl = foldl (\acc x -> acc*10 + x) 0 

--(b) Defina una función repr :: Int -> [Int], que dado un natural (de
--tipo Int) retorna su representación. Escriba dicha función utilizando
--recursión expllícita. ¿Se puede hacer utilizando foldl o foldr?

--RECURSIÓN EXPLÍCITA
reprRecExp1::Int->[Int]
reprRecExp1 x
            | x < 10 = [x]
            | otherwise = (reprRecExp1 (div x 10)) ++ [(mod x 10)] 

-- ¿Se puede hacer utilizando foldl o foldr?
--Más bien que se puede.
--El Int que te pasan se lo pasas a los folds como [Int].
--Haces funciones auxiliares que se encarguen de la recursión pedida,
--las cuales sean binarias pero no hagan un carajo con el acumulador
--que reciben.
--Por último los folds harán recursion en una lista de un único elemento,
--y todo el trabajo lo hace la función "binaria" pasada por parámetro.
--Y así demostras que sos horrible para programar, sos inimputable hermano,
--olvidate, en 10 días sos ingeniero.

--CON foldr
unAsco::Int->[Int]->[Int]
unAsco x y
        | x < 10 = [x]
        | otherwise = (reprRecExp1 (div x 10)) ++ [(mod x 10)] 

reprConFoldr::Int->[Int]
reprConFoldr x = foldr (unAsco) [] [x]
-- ¿Puede ser más asqueroso? No lo creo

--CON foldl
otroAsco::[Int]->Int->[Int]
otroAsco y x
        | x < 10 = [x]
        | otherwise = (reprRecExp1 (div x 10)) ++ [(mod x 10)] 

reprConFoldl::Int->[Int]
reprConFoldl x = foldl (otroAsco) [] [x]

--Aclaro, tal vez hay una forma de hacerlo bien con los folds, pero ¿viste cuando
--tu mente piensa la forma asquerosa de una, y no queres ponerte a pensar
--una forma elegante? bueno así.

--Bueno pensando a la carrera, tal vez se pueda convertir el Int que te pasan en un String,
--y trabajar con eso.
reprConFoldr2::Int->[Int]
reprConFoldr2 i = foldr (\x acc -> (read [x]):acc) [] (show i)

--Si se puede, pero es un asco también. 1 Asco.

--(c) Defina una función sucesor :: [Int ] -> [Int ], que dado un decimal
--en esta representación compute el siguiente decimal. Escriba dicha
--función utilizando recursión explícita y usando foldr . Se debe traba-
--jar directamente con la representación. No deben usarse las funciones
--decimal ni repr.

--RECURSIÓN EXPLÍCITA
sucersorRecExp:: [Int]->[Int]
sucersorRecExp [] = error "vacío no amigo"
sucersorRecExp (x:[]) = if x+1 < 10 then [x+1] else [1,0]
sucersorRecExp xs = if ultimo < 9 then init xs ++ [ultimo + 1] else sucersorRecExp (init xs) ++ [0]
                where ultimo = last xs


--CON foldr
sucersorConFoldr:: [Int]->[Int]
sucersorConFoldr [] = error "vacío no amigo"
sucersorConFoldr xs = foldr (\x acc -> if ((null acc) || (head acc == 10)) then [x+1]:acc else [x]:acc) [] xs

l:: [Integer]
l = [1,2,3,4,1,2,3,4]

l2 = [1,2]

l3:: [Int]
l3 = [1..10]

l4 = [1,2,2,3,4,4,4,3]
l5 = [2,4,6,8,7]

l6 = [8,10,6,10,10]

l7::[Int]
l7 = [9,9,9]