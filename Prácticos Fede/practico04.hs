-- PRÁCTICO 4 PROGRAMACIÓN FUNCIONAL 2017
import Data.Char
import Data.List

-- ********************************** 1 ***************************************
--1. Considere la siguiente definición de los números naturales:
data Nat = Zero | Succ Nat deriving (Show)

--Esta función no la piden, la hago para facilitar pruebas. (Trato negativos como si fueran ceros)
int2nat :: Int -> Nat
int2nat i
    | i > 0 = Succ (int2nat (i-1))
    | otherwise = Zero

--Creo algunos naturales...
n0 = int2nat 0
n1 = int2nat 1
n2 = int2nat 2
n5 = int2nat 5
n6 = int2nat 6
n10 = int2nat 10

-- (a) Defina las siguientes funciones sobre los naturales:

nat2int :: Nat -> Int --convierte un natural en entero
nat2int Zero = 0
nat2int (Succ n)  = 1 + nat2int n

duplica :: Nat -> Nat --doble del argumento
duplica Zero = Zero
duplica (Succ n) = Succ (Succ (duplica n))

exp2 :: Nat -> Nat --exponente en base 2
exp2 Zero = Zero
exp2 (Succ n) = int2nat (decimal2binary (nat2int (Succ n)))

decimal2binary :: Int -> Int
decimal2binary 0 = 0
decimal2binary d =  (decimal2binary (div d 2)) * 10 + (mod d 2)
 
suma :: Nat -> Nat -> Nat --suma de naturales
suma Zero n = n 
suma (Succ n1) n2 = suma n1 (Succ n2) 


predecesor :: Nat -> Nat --predecesor (predecesor de cero es cero)
predecesor Zero = Zero
predecesor (Succ n) = n

--(b) El fold para los naturales se define de la siguiente manera:
foldN :: (a -> a) -> a -> Nat -> a
foldN h e Zero = e
foldN h e (Succ n) = h (foldN h e n)

--Defina las funciones de la parte a) en función de foldN .

nat2int_foldN :: Nat -> Int
nat2int_foldN n = foldN  (\ x -> x+1) 0 n

duplica_foldN :: Nat -> Nat --doble del argumento
duplica_foldN n = foldN (\ x -> Succ (Succ x)) Zero n

--No se me ocurre la de exp, ya la anterior exp la hice feo.

suma_foldN :: Nat -> Nat -> Nat --suma de naturales
suma_foldN n1 n2 = foldN (\ x -> (Succ n2)) n2 n1

predecesor_foldN :: Nat -> Nat --predecesor (predecesor de cero es cero)
predecesor_foldN Zero = Zero
predecesor_foldN (Succ n) = foldN (\ x -> Succ x ) Zero n

--(c) Defina la función fib :: Nat -> Nat que computa los números de 
--    fibonacci. Una forma de definir esta función es por la conocida fórmula
--    de recurrencia que caracteriza estos números. Impleméntela.

fib_rec :: Nat -> Nat
fib_rec Zero = Zero
fib_rec (Succ Zero) = (Succ Zero)
fib_rec (Succ (Succ n)) = (fib_rec (Succ n)) `suma` (fib_rec n)

-- Es bien sabido que dicha definición tiene un problema: su compor-
-- tamiento es exponencial. Pruebe aplicarla con valores crecientes de
-- naturales y lo podrá verifica. Hay una mejor definición, que es lin-
-- eal, la cual utiliza una forma "iterativa" para computar los números
-- de fibonacci. Implemente esta definición alternativa.

fib_iter :: Nat -> Nat
fib_iter Zero = Zero
fib_iter (Succ Zero) = (Succ Zero)
fib_iter n = fst (foldN (\ (i,j) -> (i`suma`j, i)) ((Succ Zero),Zero) (predecesor n))


-- ********************************** 2 ***************************************
-- 2. Suponga que definimos los enteros de la siguinte forma:
data OurInt = IntZero | Pos Nat | Neg Nat
-- tal que IntZero representa el cero de los enteros, Pos los enteros positivos
-- (1,2,...) y Neg los negativos (-1,-2,...). Por ejemplo, el 2 es dado por
-- Pos (Succ Zero), mientras que el -1 por Neg Zero.

instance Show OurInt where
    show IntZero = "0" 
    show (Pos n) = show ((nat2int n)+1)
    show (Neg n) = "-" ++ show ((nat2int n)+1)


--Creo algunos enteros...
i0 = IntZero
i1 = Pos Zero
i2 = Pos (int2nat 1)
i5 = Pos (int2nat 4)
i6 = Pos (int2nat 5)
i10 = Pos (int2nat 9)

i_1 = Neg Zero
i_2 = Neg (int2nat 1)
i_5 = Neg (int2nat 4)

sumaEntera :: OurInt -> OurInt -> OurInt
sumaEntera IntZero i = i
sumaEntera i IntZero = i
-- N + N 
sumaEntera (Neg n1) (Neg n2) = restaEntera (Neg n1) (Pos n2)
-- P + N
sumaEntera (Pos n1) (Neg n2) = restaEntera (Pos n1) (Pos n2)
-- N + P
sumaEntera (Neg n1) (Pos n2) = restaEntera (Pos n2) (Pos n1)
-- P + P
sumaEntera (Pos n1) (Pos n2) = Pos (Succ (suma n1 n2))


restaEntera :: OurInt -> OurInt -> OurInt
restaEntera IntZero i = negate i
restaEntera i IntZero = i
-- N - N 
restaEntera (Neg n1) (Neg n2) = restaEntera (Pos n2) (Pos n1)
-- P - N
restaEntera (Pos n1) (Neg n2) = Pos (Succ (suma n1 n2))
-- N - P
restaEntera (Neg n1) (Pos n2) = Neg (Succ (suma n1 n2))
-- P - P
restaEntera (Pos Zero) (Pos Zero) = IntZero
restaEntera (Pos (Succ n1)) (Pos Zero) = restaEntera (Pos n1) IntZero
restaEntera (Pos Zero) (Pos (Succ n2)) = restaEntera IntZero (Pos n2)
restaEntera (Pos (Succ n1)) (Pos (Succ n2)) = restaEntera (Pos n1) (Pos n2)

multEntera :: OurInt -> OurInt -> OurInt
multEntera IntZero i = IntZero
multEntera i IntZero = IntZero
multEntera (Neg n1) (Neg n2) = foldN (\ x -> x + (Pos n1)) (Pos n1) n2
multEntera i (Pos n2) = foldN (\ x -> x + i) i n2
multEntera (Pos n1) (Neg n2) = negate (foldN (\ x -> x + (Pos n1)) (Pos n1) n2)

integer2OurInt:: Integer -> OurInt
integer2OurInt i 
    | (i == 0) = IntZero 
    | (i > 0) = Pos (integer2nat (i-1))
    | otherwise = Neg (integer2nat (i-2))

integer2nat :: Integer -> Nat
integer2nat i
    | i > 0 = Succ (integer2nat (i-1))
    | otherwise = Zero

-- (a) Defina la instancia de la clase Num para OurInt.
instance Num OurInt where

    (+) i1 i2 = sumaEntera i1 i2
    (-) i1 i2 = restaEntera i1 i2
    (*) i1 i2 = multEntera i1 i2

    negate IntZero = IntZero 
    negate (Pos n) = Neg n
    negate (Neg n) = Pos n     

    abs (Neg n) = Pos n
    abs i = i

    signum IntZero = IntZero
    signum (Pos n) = (Pos Zero)
    signum (Neg n) = (Neg Zero)

    fromInteger = integer2OurInt

-- Respondiendo las preguntas:
-- La primer alternativa tiene problemas con los signos, la segunda no representa el cero.


-- ********************************** 3 ***************************************
-- Considere la siguiente definición de árbol binario:
data Tree a = Empty | Node (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
    show Empty = "Show nope implementado" 
    show (Node izq raiz der) = "Show no implementado"

--(a) Defina las recorridas en inorder, preorder y postorder sobre un árbol
--    binario, las cuales listan los elementos del árbol en el respectivo orden.
--    Todas ellas tienen tipo Tree a -> [a].

inorder:: Tree a -> [a]
inorder Empty = []
inorder (Node izq raiz der) =
    inorder izq ++ [raiz] ++ inorder der

preorder:: Tree a -> [a]
preorder Empty = []
preorder (Node izq raiz der) =
    [raiz] ++ preorder izq ++ preorder der

postorder:: Tree a -> [a]
postorder Empty = []
postorder (Node izq raiz der) =
     postorder izq ++ postorder der ++ [raiz] 

-- (b) Defina la función mkTree :: Ord a => [a ] -> Tree a que construye un
-- árbol binario de búsqueda a partir de una lista. (El árbol generado
-- no precisa estar balanceado.)

mkTree :: Ord a => [a] -> Tree a
mkTree list = foldr (insertTree) Empty list

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree a Empty = Node Empty a Empty
insertTree a (Node izq raiz der)
    | (a<raiz) = (Node (insertTree a izq) raiz der)
    | otherwise = (Node izq raiz (insertTree a der))

--(c) Que hace la composición inorder . mkTree?
--Toma una lista en orden cualquiera, mkTree genera el árbol ordenado,
--y luego inorder genera la lista asociada al ABB, como recorre en orden,
--termina generando una lista ordenada.


asdasd:: Maybe a -> Int
asdasd a = 0

mapF f = [f 'a', f 'b']


prueba  = (-) 2 . (*) 4 . const 3
prueba2 = (*) 4 . const 3
prueba3  = (-) 2 . (*) 4 


masp = do 
    result <- fmap (map toUpper) getLine
    return (result)

mayus p = map toUpper p

eval (Add x y) = eval x >>= \a -> eval y >>= \b -> return (a + b)



-- ********************************** 4 ***************************************

-- ********************************** 5 ***************************************

-- ********************************** 6 ***************************************

-- ********************************** 7 ***************************************

-- ********************************** 8 ***************************************

-- ********************************** 9 ***************************************

-- ********************************** 10 ***************************************
