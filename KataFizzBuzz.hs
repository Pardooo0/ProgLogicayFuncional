module Main where

import Data.List (intercalate)

-- Función para ver si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n < 2 = False
    | otherwise = all (\d -> n `mod` d /= 0) [2..(floor . sqrt $ fromIntegral n)]

-- Listas de palabras para números básicos
digitos :: [String]
digitos = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]

dieces :: [String]
dieces = ["diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]

decenas :: [String]
decenas = ["veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

centenas :: [String]
centenas = ["cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]

-- Función  para convertir números a texto
numeroATexto :: Int -> String
numeroATexto 0 = "cero"
numeroATexto 1000000 = "un millón"
numeroATexto n
    | n < 10 = digitos !! n
    | n < 20 = dieces !! (n - 10)
    | n < 30 = "veinti" ++ if n == 20 then "" else numeroATexto (n `mod` 10)
    | n < 100 = let (cociente, resto) = n `divMod` 10 in decenas !! (cociente - 2) ++ if resto /= 0 then " y " ++ numeroATexto resto else ""
    | n == 100 = "cien"
    | n < 200 = "ciento " ++ numeroATexto (n - 100)
    | n < 1000 = let (cociente, resto) = n `divMod` 100 in centenas !! (cociente - 1) ++ if resto /= 0 then " " ++ numeroATexto resto else ""
    | n < 2000 = "mil " ++ if n == 1000 then "" else numeroATexto (n `mod` 1000)
    | n < 1000000 = let (cociente, resto) = n `divMod` 1000 in numeroATexto cociente ++ " mil" ++ if resto /= 0 then " " ++ numeroATexto resto else ""
    | otherwise = "Número fuera de rango"

-- Función para ver si imprimir FizzBuzz! o convertir el número a palabras
fizzBuzzOTexto :: Int -> String
fizzBuzzOTexto n = if esPrimo n then "FizzBuzz!" else numeroATexto n

main :: IO ()
main = do
    putStrLn "Ingrese un número entre 0 y 1000000:"
    entrada <- getLine
    let n = read entrada :: Int
    if n >= 0 && n <= 1000000
        then putStrLn (fizzBuzzOTexto n)
        else putStrLn "Número fuera de rango. Intente nuevamente."