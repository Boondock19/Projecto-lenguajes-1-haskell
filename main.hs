import Data.List
import System.Environment
import System.Random

import Data.List.Split
import Data.Char

contains :: Eq a => a -> [a] -> Bool
contains elem myList
  = case myList of
      [] -> False
      x : xs | x == elem -> True
      _ : xs -> contains elem xs

mayuscula :: String -> String
mayuscula []     = []
mayuscula (x:xs) = toUpper x : [toUpper x | x <- xs]

revisionV:: Int -> [[Char]] -> [[Char]] -> [Char] -> [Char]
revisionV num userWord listOfBulls stringEvaluation = do
    if num < 5 
        then do
            let charInUserWord = userWord !! num
            let charInStringEvaluation = [stringEvaluation !! num]
            if  "V" == charInStringEvaluation && contains charInUserWord listOfBulls
                then do
                    let (x,_:ys) = splitAt num stringEvaluation
                    let stringArreglado = x ++ "-" ++ ys
                    revisionV (num+1) userWord listOfBulls stringArreglado

            else revisionV (num+1) userWord listOfBulls stringEvaluation
    else do
        stringEvaluation
    

    
revisar :: Int -> [[Char]] -> [[Char]] -> [Char] -> [[Char]] -> [Char]
revisar num x randomWord lista listOfBulls= do
    let toro = ['T']
    let guion = ['-']
    let vaca = ['V']
    if num < 5
        then do
            let comp1 = x !! num
            let comp2 = randomWord !! num
            
            if comp1 == comp2 
                then do
                    let newLista =  lista ++ toro
                    let newListOfBulls = listOfBulls ++ [comp1] 
                    revisar (num+1) x randomWord newLista newListOfBulls
            else do 
                if  contains comp1 randomWord && not (contains comp1 listOfBulls)
                    then do
                        let newLista =  lista ++ vaca 
                        revisar (num+1) x randomWord newLista listOfBulls
                        
                       
                else 
                    do
                        let newLista =  lista ++ guion 
                        revisar (num+1) x randomWord newLista listOfBulls
                        
    else do
        revisionV 0 x listOfBulls lista
        

initMenteMaestra :: Int -> String -> IO ()
initMenteMaestra currentTurn randomWord = do
    if currentTurn > 6 then putStrLn ("Haz perdido , la palabra era " ++ randomWord )
    else do
        putStrLn "Por favor ingresa una palabra: "
        x <- getLine
        let x2 = mayuscula x
        let x3 = splitOn "" x2
        let lchar = drop 1 x3
        let x4 = splitOn "" randomWord
        let lrW = drop 1 x4
        --let respuesta [] = []
          
        if  length x /= 5 || contains "Ñ" lchar
            then do
                putStrLn "Has ingresado una palabra invalida"
                initMenteMaestra currentTurn randomWord
        else if x2 == randomWord 
            then putStrLn ("Haz ganado!, la palabra era " ++ randomWord )
        else do
            
            putStrLn (revisar 0 lchar lrW [] [[]])
            -- let (x,_:ys) = splitAt 1 (revisar 0 lchar lrW [] [[]])
            -- putStrLn ("Este es x: " ++ x)
            -- putStrLn ("Este es ys: " ++ ys)
            -- let stringArrglado = x ++ "-" ++ ys
            -- putStrLn ("Este es el string arreglado: " ++ stringArrglado)
            initMenteMaestra (currentTurn + 1 ) randomWord


main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are :"
    mapM_ putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
    fileContent <- readFile "Palabras.txt"
    -- Lista de todas las palabras.
    let listOfWords = lines fileContent
    let sizeOfList = length listOfWords
    -- Obtenemos una palabra aleatoria de la lista.
    index <- randomRIO (0, sizeOfList - 1)
    let randomWord =  listOfWords !! index
    
    print ("La palabra es : " ++ randomWord ++ " con index : " ++ show index)

    --print(listOfWords)

    -- Lista de puntos de las palabras
    let word = "12345"
    let ver = "TTTTT"
    let cont = 0
    

    initMenteMaestra 1 "PERDI"

    -- salir :: Integer -> String
    -- salir n = contador n
    --     where
    --         contador n
    --             | n == 6 = "Perdiste"
    --             | otherwise "Sigue"
    

    -- if word == "CINCO" then do
    --     print(ver)
    -- else do
    --     let cont = cont + 1
    --     main

    