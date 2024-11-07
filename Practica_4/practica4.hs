data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der


-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)


-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der


-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz a izq der) InOrder = recorrido izq InOrder ++ [a] ++ recorrido der InOrder
recorrido (Raiz a izq der) PreOrder = [a] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz a izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [a]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = [[]]
niveles (Raiz a ArbolVacio ArbolVacio) =[[a]] 
niveles (Raiz a izq der) = [a] : gygafunction (niveles izq) (niveles der)

-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a 
minimo ArbolVacio = error "Arbol vacio"
minimo (Raiz a ArbolVacio ArbolVacio) = a
minimo (Raiz a ArbolVacio der) = if a <= minimo der
                                    then a
                                    else minimo der
minimo (Raiz a izq ArbolVacio) = if a <= minimo izq
                                    then a
                                    else minimo izq
minimo (Raiz a izq der) = if a <= minimo izq && a <= minimo der
                            then a
                            else if minimo izq <= minimo der && minimo izq <= a
                                then minimo izq
                                else minimo der


-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a 
maximo ArbolVacio = error "Arbol vacio"
maximo (Raiz a ArbolVacio ArbolVacio) = a
maximo (Raiz a ArbolVacio der) = if a >= maximo der
                                    then a
                                    else maximo der
maximo (Raiz a izq ArbolVacio) = if a >= maximo izq
                                    then a
                                    else maximo izq
maximo (Raiz a izq der) = if a >= maximo izq && a >= maximo der
                            then a
                            else if maximo izq >= maximo der && maximo izq >= a
                                then maximo izq
                                else maximo der

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz a ArbolVacio ArbolVacio) x = if a == x
                                                then ArbolVacio
                                                else Raiz a ArbolVacio ArbolVacio
eliminar (Raiz a izq ArbolVacio) x = if x == a
                                        then izq
                                        else if x < a
                                            then Raiz a (eliminar izq x) ArbolVacio
                                            else Raiz a izq ArbolVacio
eliminar (Raiz a ArbolVacio der) x = if x == a 
                                        then der
                                        else if x > a
                                            then Raiz a ArbolVacio (eliminar der x)
                                            else Raiz a ArbolVacio der
eliminar (Raiz a izq der) x = if x == a
                                then Raiz (maximo izq) (eliminar izq (maximo izq)) der
                                else if x < a
                                    then Raiz a (eliminar izq x) der
                                    else Raiz a izq (eliminar der x)

--Funcion auxiliar para el ejercicio 5
gygafunction :: [[a]] -> [[a]] -> [[a]]
gygafunction [] [] = []
gygafunction [] (x:xs) = x : gygafunction [] xs
gygafunction (x:xs) [] = x : gygafunction xs []
gygafunction (x:xs) (y:ys) = (x ++ y) : gygafunction xs ys
