data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud = undefined

estaContenido :: Eq a => List a -> a -> Bool
estaContenido = undefined

convertirAEstructura :: [a] -> List a
convertirAEstructura = undefined

convertirALista :: List a -> [a]
convertirALista = undefined

conjunto :: Eq a => List a -> List a
conjunto = undefined

eliminarIndice :: List a -> Int -> List a
eliminarIndice = undefined

insertarIndice :: List a -> Int -> a -> List a
insertarIndice = undefined

recorrerLista :: List a -> Int -> List a
recorrerLista = undefined





