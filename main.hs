import Data.List
import System.Environment



                
printInt x = print            

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
    print listOfWords
    let turno = 1 
    sumTurno turno
    
   
    





