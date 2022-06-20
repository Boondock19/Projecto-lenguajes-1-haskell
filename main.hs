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
    print(listOfWords)
    
    

