import Data.List
import System.Environment
import Data.List.Split

contains :: Eq a => a -> [a] -> Bool
contains = \elem -> \myList ->
  case myList of
    [] -> False 
    x:xs | x == elem -> True 
    _:xs -> contains elem xs


initMenteMaestra :: Int -> String -> IO ()
initMenteMaestra currentTurn randomWord = do
    if currentTurn > 6 then putStrLn ("Haz perdido , la palabra era " ++ randomWord )
    else do
        putStrLn "Por favor ingresa una palabra: "
        x <- getLine
        let y = splitOn "" x
        let z = drop 1 y
          
        if  length x /= 5  
            then do
                putStrLn "Has ingresado una palabra invalida"
                initMenteMaestra currentTurn randomWord
        else if contains "Ñ" z || contains "ñ" z
            then do
                putStrLn "Has ingresado una palabra invalida"
                initMenteMaestra currentTurn randomWord 
        else if x == randomWord 
            then putStrLn ("Haz ganado!, la palabra era " ++ randomWord )
        else do
            putStrLn "Haz ingresado una palabra valida!"
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
    --print(listOfWords)

    -- Lista de puntos de las palabras
    let word = "12345"
    let ver = "TTTTT"
    let cont = 0
    

    initMenteMaestra 1 "ESKER"

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

    