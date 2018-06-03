-- PRÁCTICO 2 PROGRAMACIÓN FUNCIONAL 2017

--Recordatorio: Siempre que puedo y me doy cuenta utilizo pointless style.

-- ********************************** 1 ***************************************
-- 1. (a) Implemente la función map usando listas por comprensión.
map':: (a->b)->[a]->[b]
map' f xs = [ f x | x <- xs]

--(b) Implemente la función filter usando listas por comprensión.
filter':: (a->Bool)->[a]->[a]
filter' p xs = [x | x <- xs, p x]

-- ********************************** 2 ***************************************
--2. Usando map, defina una función squares :: [Int ] -> [Int ] que dada una
--   lista de enteros retorne una lista con los cuadrados de los elementos de la
--   lista.
squares:: [Int]->[Int]
squares = map (^2)

-- ********************************** 3 ***************************************
--3. Defina la función length en términos de map y sum.

--Primero defino una función auxiliar que devuelve 1 siempre que recibe un parámetro.
--La idea es mapear la lista con esta función, obteniendo así una lista de la forma
--[1,1,1....] con tantos 1's como elementos tenga la lista mapeada, luego se le aplica sum 
-- y se obtiene el largo. Puto el que lee.
aux:: a->Int
aux x = 1

length':: [a]->Int
length' xs = sum (map aux xs)

-- ********************************** 4 *************************************** 
--4. Usando filter , defina una función all :: (a -> Bool ) -> [a ] -> Bool que
--   dada una condición y una lista, verifique si todos los elementos de la lista
--   cumplen con dicha condición. Ejemplos:
all':: (a->Bool)->[a]->Bool
all' p xs 
        | length (filter p xs) == length xs = True
        | otherwise = False 

-- ********************************** 5 *************************************** 
--5. Indique el tipo y explique lo que hace la siguiente función:
--    rara p = filter p . filter (not . p)

rara:: (a->Bool)->[a]->[a]
rara p = filter p . filter (not . p)

--La función recibe como parámetro un predicado p y una lista list que contenga cualquier elemento.
--Primeramente a list se le aplica la función filter, utilizando como predicado la composición 
--de las funciones not y p. A la lista resultante de aplicar dicho filter se le vuelve a aplicar otro
--filter utilizando como predicado la función p.

--Se utiliza la técnica point free style (pointless style) o estilo libre de puntos, es por esto que
--el parámetro correspondiente a la lista no aparece en el cuerpo de la función ni en su llamada.
--Esta técnica está fundamentada en la currificación de funciones y básicamente se puede aplicar
--cuando en el lado derecho de ambos lados de la ecuación aparece el mismo parámetro.
--Ejemplo de pointless style:    foo x = length x  equivale a foo = length

--La función siempre devolverá la lista vacía, ya que primero se filtra list según not p, y luego
--se filtra el resultado según p.

--Se deduce que la función recibe una lista y un predicado, ya que filter utiliza estos tipos de parámetros.


-- ********************************** 6 *************************************** 
--6. Dada la siguiente función

dup x = (x , x )
--Explique en que difieren (dup . dup) y (dup dup).
-- Ambas hacen lo mismo, duplican un parámetro, y luego duplican el resultado ya duplicado una vez.
--Al usar composición de funciones, la variante está en que la primer expresión (dup . dup) crea 
--una nueva función que duplica el parámetro recibido, y luego duplica el resultado nuevamente,
--entonces se tendría una sola función.
--La otra expresión en cambio utiliza dos funciones claramente diferenciadas, las cuales se aplican por
--separado.
r1 = dup $ dup 5    --Equivale a dup (dup 5)   --Primero se aplica un dup y luego el otro dup.
r2 = dup . dup $ 5  --Equivale a (dup . dup) 5 --Primero se forma la función compuesta, y luego se le ingresa 5 de parámetro.



-- ********************************** 7 *************************************** 
--7. Indique el tipo y explique lo que hace la siguiente función:
--   rara2 = zipWith (.) [length, sum ] [drop 4, take 4]
--   Muestre un ejemplo de aplicación correcta de la expresión (head rara2) y
--   su resultado.

--rara2:: [a]
rara2 = zipWith (.) [length, sum] [drop 4, take 4]

--Toma las dos listas de funciones y devuelve una lista con las funciones compuestas cuyos índices
--son iguales.
--1) drop 4 y take 4 son de tipo [a]->[a]
--2) lenght es de tipo [a]->Int
--3) sum es de tipo [Int]->Int
--4) Como sum se compone con take 4, por 3) se restringe que take 4 sea de tipo [Int]->[Int]
--Como drop 4 pertenece a la misma lista que take 4, drop 4 debe ser del tipo [Int]->[Int] por 4)
--Como length pertenece a la misma lista que sum, length debe ser del tipo [Int]->Int por 3)
--Es así que la lista resultado, estará formada por elementos que son compuestos por los tipos:
-- [Int]->[Int] y [Int]->Int, lo que en definitiva es el tipo [Int]->Int
--Como fue dicho antes, se devuelve una lista, entonces rara2:: [[Int]->Int]

--La función head retorna el primer elemento de una lista, aplicando head rara2 obtendríamos
--la función compuesta: length . drop 4
--Aplicación correcta: (head rara2) [Int]
x = (head rara2) [1..10]


-- ********************************** 8 *************************************** 
--8. Dada la función twice
--   twice f = f . f
--   Explique el resultado de hacer twice tail [1, 2, 3, 4]. ¿Es posible hacer
--   twice head [1,2,3,4]? Justifique.

--tail retorna la lista sin la cabeza.
--head retorna la el primer elemento (no en forma de lista).

--La aplicación de funciones tiene alto orden de presedencia (asociativa por izquierda)
--Entonces:       twice tail [1,2,3,4] = (twice tail) [1,2,3,4]
--Por lo tanto la expresión resultaría tail . tail [1,2,3,4]
--Entonces se aplica tail a [1,2,3,4], retornando [2,3,4]
--Luego se aplica el segundo tail a [2,3,4] retornando [3,4]

--No es posible hacer twice head [1,2,3,4].
--Siguiendo el mismo razonamiento que en la parte anterior, llegamos a
--                head . head [1,2,3,4]
--Al aplicar el primer head a [1,2,3,4] obtenemos como resultado 1
--Luego no se puede aplicar el segundo head a 1 ya que este no es una lista.

twicee f = f . f


-- ********************************** 9 ***************************************
-- 9. La función 
--    ip tiene el siguiente tipo: (a -> b -> c) -> b -> a -> c.
--    Observando el tipo, ¿puede determinar qué hace la función? ¿Puede implementarla?

--No se puede determinar qué hace flip especificamente, pero si se sabe que la función flip recibe como parámetro
--otra función la cual a su vez recibe dos parametros de tipo a y b y devuelve un c.
--Luego la función flip retorna otra función que toma b y a y devuelve un valor de tipo c.


-- ********************************** 10 ***************************************
-- 10. (a) Utilizando flip, mod, length, map y filter , defina una función que
--     dada una lista de enteros retorne la cantidad de elementos pares que
--     tiene la lista.
--     (b) Haga lo mismo, pero sin usar map.
--     (c) Haga lo mismo, pero sin usar flip.

a::[Int]->Int
a xs = length $ filter ( ==0) $ flip map xs (mod 2)


b::[Int]->Int
b xs = length (flip filter [ x `mod` 2 | x <- xs] ( ==0))

c::[Int]->Int
c xs = length . filter ( ==0) . map (mod 2) $ xs

c_lindo::[Int]->Int
c_lindo xs =
    let mapeadoConMod2 = map (mod 2) xs
        pares = filter (==0) mapeadoConMod2
        cantidadDePares = length pares
    in cantidadDePares

-- ********************************** 11 ***************************************
--11. Sea h x y = f (g x y). ¿Cuáles de las siguientes afirmaciones son correctas?
--    (a) h ≡  f . g
--    (b) h x ≡  f . g x
--    (c) h x y ≡  (f . g) x y

-- (a) Falso, no pueden desaparecer 2 parámetros. Solo puedo sacar uno con pointless style.
-- (b) Verdadero
-- (c) Falso
-- Fue mas bien a prueba y error, no se las justificaciones correctas.

g x y = x + y
f x = x + 1
hola x y = f(g x y)
hola2 x = f . g x
hola3 x y =  f . g x $ y

-- ********************************** 12 ***************************************
--12. Se puede definir la función filter en términos de concat y map:
--    filter p = concat . map box
--              where box x = ...
--    Dar la defincición de box.

filter2:: (a->Bool)->[a]->[a]
filter2 p = concat . map box 
         where box x = if p x then [x] else []
--NOTA: Recordar que me complicó mucho intentar hacer a box como una función así: [[x] | x<-xs, p x]
--El error estaba en que map ya recorre la lista que le pasan, y aplica la función a cada elemento de ella,
--entonces pasarle una función que recorriera una lista obligaba a que el parámetro de map fuera una lista
--de listas, lo cual era un error.


-- ********************************** 13 ***************************************
--13. Explique por qué la siguiente definición no es aceptada por el sistema de
--tipos de Haskell:
--dobleAp f = (f True, f 'a')
--Lo que intenta hacer dobleAp, es recibir una función y devolver una dupla cuyos elementos
--son la función recibida aplicada a 2 valores de tipos distintos, supongo que el error es ese.

--Cosas para probar
l:: [Int]
l = [1,2,3,4]
l2 = [1,2]
l3 = [1..10]

p = (>2)