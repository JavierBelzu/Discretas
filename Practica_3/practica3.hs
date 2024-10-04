data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node c xs) = 1 + longitud xs

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node c xs) y = c == y || estaContenido xs y

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node c xs) = c : convertirALista xs

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node c xs) = if estaContenido xs c
                        then conjunto xs
                        else Node c (conjunto xs)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = Void
eliminarIndice (Node c xs) y = if y >= 0 && y < longitud (Node c xs)
                                then if y == 0 
                                    then xs 
                                    else Node c (eliminarIndice xs (y-1))
                                else error "Indice fuera del rango permitido"

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void x y = Node y Void
insertarIndice (Node c xs) 0 y = Node y (Node c xs)
insertarIndice (Node c xs) x y = if x>= 0 && x <= longitud (Node c xs)
                                    then Node c (insertarIndice xs (x-1) y)
                                    else error "Indice fuera del rango permitido"

recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void
recorrerLista (Node c xs) 0 = Node c xs
recorrerLista (Node c xs) y = recorrerLista (insertarIndice xs (longitud xs) c) (y-1)




