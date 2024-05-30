module Caesar where
import Data.Char


-- Codigo Caesar



-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula x | (ord x < 97 ) || (ord x > 122) = False
              | otherwise = True


-- EJ 2  
letraANatural :: Char -> Int
letraANatural x | ord x == 97 = 0
                | otherwise = letraANatural (chr (ord x -1)) + 1


-- EJ 3

desplazar :: Char -> Int -> Char
desplazar x y 
    | esMinuscula x == False = x 
    | ubiASCII x y > 122 = chr (ubiASCII x y - 26)
    | ubiASCII x y < 97 = chr (ubiASCII x y + 26)
    | otherwise = chr (ubiASCII x y)

ubiASCII :: Char -> Int -> Int       
ubiASCII x y = (ord x) + mod y 26
            


-- EJ 4
cifrar :: String -> Int -> String
cifrar x 0 = x
cifrar [] y = []
cifrar (x:xs) y =  (desplazar x y) : cifrar (xs) y



-- EJ 5
descifrar :: String -> Int -> String
descifrar x 0 = x
descifrar [] y = []
descifrar (x:xs) y = (desplazar x (-y)) : descifrar (xs) y




-- EJ 6

cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (x:xs) = cifrarListaAux (x:xs) 0


cifrarListaAux :: [String] -> Int ->[String]
cifrarListaAux [] _ = []
cifrarListaAux (x:xs) numero = cifrar x numero : cifrarListaAux xs (numero + 1)



-- EJ 7


frecuencia :: String -> [Float]
frecuencia [] = []
frecuencia (x:xs) = frecuenciasPonderadas (listaAbecedario ('a')) (x:xs)

frecuenciasPonderadas :: [Char] -> [Char] -> [Float]
frecuenciasPonderadas [] _ = []
frecuenciasPonderadas (x:xs) (y:ys) = cantidadDeAparicionesPonderada x (y:ys) : frecuenciasPonderadas xs (y:ys)

cantidadDeAparicionesPonderada :: Char -> [Char] -> Float
cantidadDeAparicionesPonderada c x = (fromIntegral (cantidadDeApariciones (c) (x)) / fromIntegral (longitud (x)))*100

cantidadDeApariciones :: Char -> [Char] -> Int
cantidadDeApariciones c x | x == [] = 0
                          | (ord (c)) > 122 || ord (c)< 97 = 0
                          | (ord (head(x)) == ord (c)) = 1 + cantidadDeApariciones c (tail (x))
                          | otherwise = 0 + cantidadDeApariciones c (tail (x))

longitud :: [t] -> Int
longitud [] = 0
longitud (x : xs)= 1 + longitud (xs)
 
listaAbecedario :: Char -> [Char]
listaAbecedario 'z' = ['z']
listaAbecedario letra = letra : (listaAbecedario (chr(ord letra +1)))





-- Ej 8


cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente x y = (chr (97 + primerMaximo (masFrecuente (frecuencia(cifrar x y))) (frecuencia (cifrar x y))), masFrecuente (frecuencia(cifrar x y)))

maximo :: [Float]->Float
maximo [x] = x
maximo (x:y:xs) | x>y = maximo (x:xs)
                | otherwise = maximo (y:xs)

masFrecuente :: [Float] -> Float
masFrecuente frecuencia = maximo (frecuencia)

primerMaximo :: Float -> [Float] -> Int
primerMaximo masFrecuente listacifrada | masFrecuente == head (listacifrada) = 0
                                       | otherwise = 1 + primerMaximo masFrecuente (tail (listacifrada))




-- EJ 9

esDescifrado :: String -> String -> Bool
esDescifrado x y | longitud x /= longitud y = False
                 | x /= descifrar (y) (posibleDesplazamiento x y) = False
                 | otherwise = True

posibleDesplazamiento :: String -> String -> Int
posibleDesplazamiento "" "" = 1
posibleDesplazamiento (x:xs) (y:ys)| esMinuscula(x) == True && esMinuscula(y) == True = ord y - ord x
                          | otherwise = posibleDesplazamiento xs ys




-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados [x] = []
todosLosDescifrados (x:y:xs) = habilitaMemoria (x:y:xs) (x:y:xs)

habilitaMemoria :: [String] -> [String] -> [(String, String)]
habilitaMemoria [x] (a:b:cs) = duplasDeLosDescifrados x (a:b:cs)
habilitaMemoria (x:y:xs) (a:b:cs) = duplasDeLosDescifrados x (a:b:cs) ++ habilitaMemoria (y:xs) (a:b:cs)

duplasDeLosDescifrados :: String -> [String] -> [(String, String)]
duplasDeLosDescifrados b [x] = []
duplasDeLosDescifrados b (x:y:xs) | b == x = duplasDeLosDescifrados b (y:xs)
                  | esDescifrado b x == True = [(b,x)] ++ duplasDeLosDescifrados b (y:xs)
                  | otherwise = duplasDeLosDescifrados b (y:xs)







