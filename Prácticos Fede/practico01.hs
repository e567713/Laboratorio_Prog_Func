import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Expr hiding (Infix)
import Text.Parsec.Token
import qualified Text.Parsec.Expr as P
import Data.Char

import Data.List

-- PRÁCTICO 1 PROGRAMACIÓN FUNCIONAL 2017

-- ********************************** 1 ***************************************
--    Defina una función sumsqrs que tome 3 números y retorne la suma 
--    de los cuadrados de los dos mayores.
sumsqrs :: (Num a, Ord a) => a -> a -> a -> a
sumsqrs x y z 
    | (x <= y) && (x <= z) = y^2 + z^2
    | (y <= x) && (y <= z) = x^2 + z^2
    | (z <= y) && (z <= x) = y^2 + x^2


-- ********************************** 2 ***************************************
-- 2. Defina and y or usando expresiones condicionales. Haga lo mismo utilizando
--    pattern matching.

-- Con guards
and' :: Bool -> Bool -> Bool
and' a b
    | ((a == b) == True) = True
    | otherwise = False 

or' :: Bool -> Bool -> Bool
or' a b
    | (a == True) = True
    | (b == True) = True
    | otherwise = False

-- Con pattern matching
and'' :: Bool -> Bool -> Bool
and'' a True = a
and'' True a = a
and'' _ _ = False

or'' :: Bool -> Bool -> Bool
or'' True _ = True
or'' _ True = True
or'' _ _ = False

-- ********************************** 3 ***************************************
-- 3. Defina al conectivo lógico implicación como un operador de tipo Bool.
implicacion :: Bool -> Bool -> Bool
implicacion True False = False
implicacion _ _ = True

-- ********************************** 4 ***************************************
-- 4. Supongamos que representamos fechas a traves de una tripla de enteros
--    que corresponden a día, mes y año. Defina una Función edad que dada
--    dos fechas, una representando la fecha de nacimiento de una persona, y la
--    otra representando la fecha actual, calcula la edad en años de la persona.

edad :: (Integral a) => (a,a,a) -> (a,a,a) -> a
edad (anio_actual, mes_actual, dia_actual) (anio_nac, mes_nac, dia_nac)
    | (mesRes < 0) =  anioRes - 1
    | ((mesRes == 0) && (diaRes < 0)) = anioRes - 1
    | otherwise = anioRes
    where anioRes = anio_actual - anio_nac
          mesRes = mes_actual - mes_nac
          diaRes = dia_actual - dia_nac

-- ********************************** 5 ***************************************
--  Se desea procesar información relativa a estudiantes. Cada estudiante está
--  dado por su nombre (cadena de caracteres), CI (entero), año de ingreso
--  (entero) y lista de cursos aprobados. Cada curso está dado por el nombre
--  del curso (cadena de caracteres), código del curso (entero) y nota de
--  aprobación (entero).

-- (a) Represente la información de cada estudiante a través de tuplas.

--Utilizo sinónimos para que quede más entendible, pero no lo pide el ejercicio
type Estudiante = (Nombre, CI, AnioIngreso, CursosAprobados)
type Nombre = String
type CI = Int
type AnioIngreso = Int
type CursosAprobados = [Curso]

type Curso = (Nombre, Codigo, Nota)
type Codigo = Int
type Nota = Int

-- (b) Escriba una función que dado un estudiante retorne su nombre y CI.
getNombreAndCI :: Estudiante -> (Nombre, CI)
getNombreAndCI (n,c,_,_) = (n,c)

-- (c) Escriba una función que dado un estudiante retorne su año de ingreso.
getAnio :: Estudiante -> AnioIngreso
getAnio (_,_,a,_) = a

-- (d) Escriba una función que dado un estudiante y una nota retorne una
--     lista con los códigos de los cursos que aprobados con esa nota. (Sugerencia:
--     use compresión de listas).
getNota :: Curso -> Nota
getNota (_,_,n) = n

cursosAprobadosConNota :: Estudiante -> Nota -> CursosAprobados
cursosAprobadosConNota (_,_,_,cursos) n = [x | x <- cursos,  getNota x == n]

--(e) Escriba una función que dada una lista de estudiantes retorne una
--    lista de pares (nombre, CI) de aquellos estudiantes ingresados en
--    un determinado año dado como parámetro. (Sugerencia: use comprensi
--    ón de listas).

ingresadosEnAnio :: [Estudiante] -> AnioIngreso -> [(Nombre, CI)]
ingresadosEnAnio xs a = [getNombreAndCI x | x <- xs, getAnio x == a]



-- ********************************** 6 ***************************************
-- 6. Rehaga el ejercicio anterior usando ahora tipos de datos algebraicos en
--    lugar de tuplas.

type CursosAprobados' = [Data_Curso]
data Data_Curso = Curso {
                          nombreCurso :: Nombre 
                        , codigo :: Codigo
                        , nota :: Nota 
                        } deriving(Show)

data Data_Estudiante = Estudiante {
                                     nombreEstudiante :: Nombre
                                   , ci :: CI 
                                   , anioIngreso :: AnioIngreso 
                                   , cursosAprobados :: CursosAprobados' 
                                   } deriving(Show)

cursosAprobadosConNota' :: Data_Estudiante -> Nota -> CursosAprobados'
cursosAprobadosConNota' (Estudiante _ _ _ cursos) n = [x | x <- cursos,  nota x == n]

ingresadosEnAnio' :: [Data_Estudiante] -> AnioIngreso -> [(Nombre, CI)]
ingresadosEnAnio' xs a = [(nombreEstudiante x, ci x) | x <- xs, anioIngreso x == a]

-- ********************************** 7 ***************************************
-- 7. Deseamos representar pares ordenados, que son pares de números reales
--    tales que el primero es menor o igual al segundo.

-- (a) Defina el tipo de los pares ordenados
data Pares = Pares { primero :: Float,
                     segundo :: Float
                   } deriving(Show, Eq)
    
-- (b) Defina una función que dado un par de reales cualesquiera retorna
--     un par ordenado.
ordenarPar:: Float -> Float -> Pares
ordenarPar x y 
    | x > y = Pares y x
    | otherwise =  Pares x y

-- (c) Defina la operación de suma de pares ordenados, que suma las correspondientes
--     componentes de dos pares retornando un nuevo par.
sumarPar:: Pares -> Pares -> Pares
sumarPar par1 par2 = Pares (primero par1 + primero par2) (segundo par1 + segundo par2)

--(d) Defina la operación de multiplicación por un escalar, que dado un
--    real y un par ordenado multiplica la primera componente del par por
--    el escalar. El resultado debe ser un par ordenado. Si se pierde el
--    orden se deben intercambiar las componentes.
multiplicarEscalar:: Float -> Pares -> Pares
multiplicarEscalar esc par
        | new > seg = Pares seg new
        | otherwise = Pares new seg
        where
            seg = segundo par
            new = primero par * esc

-- ********************************** 8 ***************************************
-- 8. Todo número entero x se puede descomponer de manera única en términos
--    de dos números enteros y y z , tales que:
--           abs(y) <= 5
--           x = y + 10 * z.
-- Defina una función que dado un entero x devuelve una tupla con los
-- números y y z .
desc:: Int -> (Int, Int)
desc x
      | y <= 5 = (y, z)
      | otherwise = (y-10, z)
      where
          y = mod x 10
          z = div x 10

-- ********************************** 9 ***************************************
-- 9. Deseamos representar números racionales y operaciones sobre ellos. Los
--    racionales son representados por pares de enteros cuya segunda componente
--    es distinta de cero. Cada racional tiene infinitas representaciones,
--    pero existe la llamada representación canónica en la que la segunda componente
--    del par de enteros es mayor que cero y ambos enteros son primos
--    entre si.

-- ADVERTENCIA, ME FUÍ DANDO CUENTA DE ERRORES LUEGO DE TERMINARLO, ENTONCES
-- ESTÁ LLENO DE CHANCHUJOS EL CÓDIGO DE ESTE EJERCICIO


-- (a) Defina el tipo racional
data Signo = Positivo | Negativo deriving(Eq)
--Definitivamente no se si el campo Signo sirve jaja, en un momento si pero cambie muchas cosas
data Racional = Racional { 
                           signo :: Signo,
                           primerComp :: Int,
                           segundaComp :: Int
                         } 
instance Show Racional where
    show (Racional sig pri seg) = (show pri) ++ if (seg /= 0) then "/" ++ (show seg) else ""

--(b) Defina una función que dado un par de enteros retorne un racional
--    en su representación canónica.
crearRacional:: Int -> Int -> Racional
crearRacional x y 
                | parametrosCorrectos && (x==0) = Racional signo (abs x) 0
                | parametrosCorrectos && (signo == Negativo) = Racional signo (-(div (abs x) mcd)) (div (abs y) mcd)
                | parametrosCorrectos = Racional signo (div (abs x) mcd) (div (abs y) mcd)
                | otherwise = error "Los parametros ingresados son incorrectos"
                where
                    mcd = gcd x y
                    parametrosCorrectos = (y /= 0)
                    signo = if ((x >= 0) && (y > 0)) || ((x <= 0) && (y < 0)) then Positivo else Negativo

--(c) Defina las operaciones de suma, resta, multiplicación, y negación de
--    racionales, e int2rac, que convierte un entero en un racional. Dichas
--    operaciones deben devolver representaciones canónicas como resultado.   
sumarRacionales:: Racional -> Racional -> Racional
sumarRacionales r1 r2 
                    | mcd > 1 = crearRacional (div primera mcd) (div segunda mcd)
                    | otherwise = crearRacional primera segunda
                    where
                        primera = (primerComp r1 * segundaComp r2 + primerComp r2 * segundaComp r1)
                        segunda = (segundaComp r1 * segundaComp r2)
                        mcd = gcd primera segunda

-- Negación??? Supuse que era cambiar el signo del racional
negarRacional:: Racional -> Racional
negarRacional (Racional sig pri sec)
                                | (sig == Positivo) = crearRacional (-(abs pri)) (abs sec) 
                                | otherwise = crearRacional (abs pri) (abs sec) 
 
--Kappa
restarRacionales:: Racional -> Racional -> Racional
restarRacionales r1 r2 = sumarRacionales r1 (negarRacional r2)

multiplicarRacionales:: Racional -> Racional -> Racional
multiplicarRacionales r1 r2
                    | mcd > 1 = crearRacional (div primera mcd) (div segunda mcd)
                    | otherwise = crearRacional primera segunda
                    where
                        primera = (primerComp r1 * primerComp r2)
                        segunda = (segundaComp r1 * segundaComp r2)
                        mcd = gcd primera segunda


-- Fracciones para hacer pruebas                    
r1 = crearRacional 5 4
r2 = crearRacional 5 (-4)
r3 = crearRacional 2 3
r4 = crearRacional 3 2


-- ********************************** 10 ***************************************
-- 10. Los lados de un triángulo cumplen la propiedad de que el lado mayor es
--     menor que la suma de los otros dos lados. Defina una función
--              esTriang :: Int -> Int -> Int -> TipoTr
--    que tome 3 números positivos en orden no decreciente y retorne un valor
--    del tipo algebraico TipoTr (a definir) el cual tiene los siguientes casos,
--    cada uno conteniendo determinada información:

--     * error, en caso que los números no representen lados de un triángulo
--     * equilátero y el valor del lado
--     * isósceles y el valor de dos lados diferentes
--     * escaleno y el valor de los tres lados.

data TipoTr = ErrorDesc {descripcion:: String}
            | Equilatero {lado:: Int}
            | Isosceles {ladosIguales, ladoDistinto:: Int}
            | Escaleno {lado1, lado2, lado3::Int}
            deriving(Show)

medianox:: Int -> Int -> Int -> Int
medianox x y z 
            | (x>=y) && (x<=z) = x
            | (y>=x) && (y<=z) = y
            | otherwise = z

-- Por las dudas me defiendo si vienen 3 lados en orden decreciente
esTriang:: Int -> Int -> Int -> TipoTr
esTriang x y z 
            | decreciente = ErrorDesc "Parametros incorrectos, no los pongas en orden decreciente papu"
            | invalidArguments = ErrorDesc "Parametros incorrectos, no forman un triangulo titan"
            | esEqui = Equilatero x
            | esEsca = Escaleno x y z
            | otherwise = Isosceles menor mayor
            where
                decreciente = (x > y) && (y > z)
                mayor = maximum [x,y,z]
                menor = minimum [x,y,z]
                mediano = medianox x y z
                invalidArguments =  (menor == 0) || (mayor > menor + mediano)
                esEqui = (x == y) && (y == z)
                esEsca = (x /= y) && (x/=z) && (y/=z)





repetidos:: [String] -> [String]
repetidos [] = []
repetidos (x:xs) 
    | null xs = [] 
    | x == head xs = [x]
    | otherwise = (repetidos (x:(tail xs))) ++ repetidos xs

l = ["f","g","f","f","g"]




pruebaa:: [(Int,Int)] -> Bool
pruebaa ((a,b): [(x,y)] ) = if a == x then True else False

l2 :: [(Int,Int)]
l2 = [(1,1),(1,2)]

  

