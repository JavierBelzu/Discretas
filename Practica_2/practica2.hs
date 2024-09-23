--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento list a b = if b
                    then a : list
                    else list ++ [a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = if x > maximoLista xs
                    then x
                    else maximoLista xs
                    
indice :: [a] -> Int -> a
indice [] a = error "La lista está vacía"
indice (x:xs) a = if a >= 0 && a <= longitud (x:xs) - 1
                    then if a == 0
                        then x
                        else indice xs (a-1)
                    else error "El indice no es valido"

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], x `mod` y == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [x] = [x]
conjunto (x:xs) = x : [y | y <- conjunto xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares (x:xs) = [y | y <- (x:xs), y `mod` 2 == 0]