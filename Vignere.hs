module Vignere where

import Caesar
import Data.Char

--Autores:

--Rutger, Pablo
--Batelli, Matias
--Bejarano, Kevin
-- Gleiss, Juan


-- EJ 1

expandirClave :: String -> Int -> String
expandirClave clave n = expandirClaveAuxiliar clave clave n 

expandirClaveAuxiliar :: String -> String -> Int -> String
expandirClaveAuxiliar _ _ 0 = []
expandirClaveAuxiliar clave [] n = expandirClaveAuxiliar clave clave n
expandirClaveAuxiliar clave (x:xs) n = x : expandirClaveAuxiliar clave xs (n-1)
 



-- EJ 2

cifrarVigenere :: String -> String -> String 
cifrarVigenere [] _ = ""
cifrarVigenere str [] = str
cifrarVigenere str c = varianteDeDesplazar (head str) (ord (head d))  : cifrarVigenere (tail str) (tail d)
                            where d = expandirClave c (length str)

varianteDeDesplazar :: Char -> Int -> Char
varianteDeDesplazar x y  | y > 0 = chr ((((ord x - ord 'a') + ( y - ord 'a') )`mod` 26) + ord 'a')
                | y < 0 = chr ((((ord x - ord 'a') - ( - y - ord 'a') )`mod` 26) + ord 'a')



-- EJ 3
descifrarVigenere :: String -> String -> String 
descifrarVigenere [] _ =  []
descifrarVigenere str [] = str  
descifrarVigenere str c  = varianteDeDesplazar (head str) (-(ord (head d))) : descifrarVigenere (tail str) (tail d)
                            where d = expandirClave c (length str)


-- EJ 4

calculoDeDistancia :: String -> String -> Int 
calculoDeDistancia _ [] = 0
calculoDeDistancia [] _ = 0
calculoDeDistancia (x:xs) (y:ys) = abs (letraANatural x - letraANatural y) + calculoDeDistancia xs ys

listaDeTuplas :: String-> [String] -> [(String,Int)]
listaDeTuplas _ [] = []
listaDeTuplas str (x:xs) = [(x,calculoDeDistancia str (cifrarVigenere str x ))] ++ listaDeTuplas str xs

seleccionaTuplas :: [(String,Int)] -> (String,Int)
seleccionaTuplas [a] = a 
seleccionaTuplas (x:y:ys) | snd x < snd y = seleccionaTuplas (x:ys)
            | otherwise = seleccionaTuplas (y:ys)

peorCifrado :: String -> [String] -> String
peorCifrado x lista = fst (seleccionaTuplas (listaDeTuplas x lista))



-- EJ 5
combinacionesVigenere :: [String]->[String]-> String -> [(String, String)]
combinacionesVigenere [] _ _  = []
combinacionesVigenere (x:xs) claves str = compara x claves str ++ combinacionesVigenere xs claves str 

compara :: String -> [String]-> String -> [(String, String)]
compara _ [] _ = []
compara str (x:xs) cifrado  | cifrarVigenere str x == cifrado  = [(str, x)] ++ compara str xs cifrado 
                            | otherwise = compara str xs cifrado 
