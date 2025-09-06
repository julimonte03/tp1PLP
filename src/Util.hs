module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = replicate (max 0 (n - length s)) ' ' ++ s
-- querriamos hacer (espacios necesarios) ++ s, donde espacios necesarios puede ser 0 si la len(palabra) >= n. Entonces tomamos el max entre 0 y (n - len(palabra)) y usamos replicate para generar esa cantidad de espacios. Luego concatenamos

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = actualizarElem' f xs n

actualizarElem' :: (a -> a) -> [a] -> Int -> [a]
actualizarElem' f = foldr (\x rec i -> if i == 0  then f x : rec (i - 1) 
                                       else x   : rec (i - 1)) (const [])

-- foldr recorre la lista de derecha a izquierda. La función que le pasamos recibe el elemento actual (x), una función rec que es la recursión (que recibe el índice i) y el índice i. Si i es 0, aplicamos f a x y luego llamamos a rec con i-1 para seguir con el resto de la lista. Si i no es 0, simplemente dejamos x igual y llamamos a rec con i-1.


-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
