import Data.List
import System.Environment
import System.Random




initMenteMaestra :: Int -> String -> IO ()
initMenteMaestra currentTurn randomWord = do
    if currentTurn > 6 then putStrLn ("Haz perdido , la palabra era " ++ randomWord )
    else do
         putStrLn "Por favor ingresa una palabra: "
         x <- getLine

          
         if  length x /= 5  
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
    

    initMenteMaestra 1 randomWord

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
    

    
