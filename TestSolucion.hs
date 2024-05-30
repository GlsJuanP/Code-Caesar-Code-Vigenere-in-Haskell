module TestSols where

import Test.HUnit
import Caesar
import Vignere
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests


allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    esMinuscula 'd' ~?= True,
    esMinuscula '1' ~?= False,
    esMinuscula '{' ~?= False
    ]

testsEjletraANatural = test [
    letraANatural 'b' ~?= 1,
    letraANatural 'a' ~?= 0,
    letraANatural 'z' ~?= 25
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd',
    desplazar 'a' (-2) ~?= 'y',
    desplazar 'f' (-52) ~?= 'f',
    desplazar '{' (-27) ~?= '{'
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq",
    cifrar "frpsxwdflrq" 23 ~?= "computacion",
    cifrar "aprobanosprofe" 52 ~?= "aprobanosprofe",
    cifrar "hoLa" 2 ~?= "jqLc"
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion",
    descifrar "computacion" 23 ~?= "frpsxwdflrq",
    descifrar "holamundo" 52 ~?= "holamundo",
    descifrar "s1lvaR" 89 ~?= "h1akpR"
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    cifrarLista ["compU", "{abo", "intr0"] ~?= ["compU", "{bcp", "kpvt0"]
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "estoycansadojefe") [12.5,0.0,6.25,6.25,18.75,6.25,0.0,0.0,0.0,6.25,0.0,0.0,0.0,6.25,12.5,0.0,0.0,0.0,12.5,6.25,0.0,0.0,0.0,0.0,6.25,0.0],
    expectlistProximity (frecuencia "Da{0") [25.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336),
    cifradoMasFrecuente "ap{{" 2 ~?= ('c', 25.00),
    expectAnyTuplaAprox (cifradoMasFrecuente "DEa777" 27) [('b', 16.66666)],
    expectAnyTuplaAprox (cifradoMasFrecuente "mama" 3) [('p', 50.0), ('d', 50.0)]
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    esDescifrado "bnbzbnkz" "cocacola" ~?= True,
    esDescifrado "Cfq3k s" "Cal3f n" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],
    expectPermutacion (todosLosDescifrados ["hola", "ipmb", "hqnv"]) [("hola", "ipmb"), ("ipmb", "hola")]
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    expandirClave "maradona" 1 ~?= "m",
    expandirClave "w" 3 ~?= "www",
    expandirClave "void" 0 ~?= ""
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    cifrarVigenere "" "ip" ~?= "",
    cifrarVigenere "hola" "mundo" ~?= "tiyd",
    cifrarVigenere "lmt" "ipc" ~?= "tbv"
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    descifrarVigenere "" "ip" ~?= "",
    descifrarVigenere "tiyd" "mundo" ~?= "hola",
    descifrarVigenere "tbv" "ipc" ~?= "lmt"
    ]

testsEjpeorCifrado = test [
    peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    peorCifrado "vocales" ["aei", "eio", "iou"] ~?= "aei",
    peorCifrado "abc" ["a", "b", "c"] ~?= "a",
    peorCifrado "xyz" ["x", "y", "z"] ~?= "z"
    ]

testsEjcombinacionesVigenere = test [
    combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    combinacionesVigenere ["maquina", "touring"] ["aprobo", "eltp"] "condiez" ~?= [],
    combinacionesVigenere ["maquina", "emb"] ["c", "b"] "god" ~?= [("emb","c")]
    ]
--}
-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)

