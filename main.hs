import Data.List
import System.Environment


main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are :"
    mapM putStrLn args
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

    salir :: Integer -> String
    salir n = contador n
        where
            contador n
                | n == 6 = "Perdiste"
                | otherwise "Sigue"
    

    if word == "CINCO" then do
        print(ver)
    else do
        let cont = cont + 1
        main
    

    