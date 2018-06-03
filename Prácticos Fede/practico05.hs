-- PRÁCTICO 5 PROGRAMACIÓN FUNCIONAL 2017

from n = n : from (n + 1)
nats = from 0

lista1 = map snd (zip nats (1 : lista1 ))

a = 1 : foldr (\x xs -> 1 + x : xs) [ ] a
b =  foldr (\x xs -> 1 + x : xs) [ ] b
c = 1 : foldl (\xs x -> 1 + x : xs) [ ] c
d = foldl (\xs x -> 1 + x : xs) [ ] d

data Tree = Empty | Node Tree Tree
data Nat = Z | S Nat
instance Eq Nat where
    Z == Z = True
    S n == S m = n == m
    _ == _ = False
eqzz t t0 = zig t == zag t0
zig Empty = Z
zig (Node l r ) = S (zag l)
zag Empty = Z
zag (Node l r ) = S (zig r )
t1 = Node (Node Empty Empty) (Node t1 Empty)
t2 = Node t2 (Node t2 Empty)
t3 = Node t3 Empty


minimo :: Ord a => [a ] -> (a,Int)
minimo (x:xs) = foldr (\e (m,i) -> if (e < m) then (e,1) else (if (e==m) then (m,i+1) else (m,i))) (x,1) xs

minimo2 :: Ord a => [a ] -> (a,Int)
minimo2 (x:xs) = foldr step (x,1) xs
    where
        step x (xs,a)
            | (x<xs) = (x,1)
            | (x==xs) = (x,1+a)
            | otherwise = (xs,a)


compone f g x y = (curry g . uncurry f ) (x , True) y

raa = (compone (curry (uncurry (&&))) fst False 2)

roo= (compone (curry snd) fst 3 (False, 5))


data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)
genera = fst $ generaAux 0
    where generaAux n = let ( l, n' ) = generaAux ( n+1 )
                            ( r, n'' ) = generaAux ( n' )
                        in (Nodo l n r, n'')
 
recorreL (Nodo l x _ )  = x : recorreL l 
recorreL Vacio             = [ ]
recorreR (Nodo _ x r ) = x : recorreR r
recorreR Vacio            = [ ]
recorre   (Nodo l x r )  = recorre l ++ [ x ] ++ recorre r
recorre   Vacio            = [ ]

ffl p = foldl (\ys x -> if p x then x : ys else ys ) [ ]
inf = 1 + inf
