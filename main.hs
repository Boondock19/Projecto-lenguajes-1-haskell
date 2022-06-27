import Data.List
import System.Environment
import System.Random
import Data.List(sort)
import Data.List.Split
import Data.Char




    

{--
    Funcion que verifica si un string contiene el caracter especificado

--}

contains :: Eq a => a -> [a] -> Bool
contains elem myList
  = case myList of
      [] -> False
      x : xs | x == elem -> True
      _ : xs -> contains elem xs

{--
    Funcion que transforma todas las letras de un string
    en mayusculas.
--}

splitAllWords :: [String] -> Int ->  Int -> [[[Char]]] -> [[[Char]]]
splitAllWords allWords position size splitWords = do
    if position == size then do
        splitWords
    else do
        let word = allWords !! position
        let x3 = splitOn "" word
        let lchar = drop 1 x3
        let newSplitWords = splitWords ++ [lchar]
        splitAllWords allWords (position + 1) size newSplitWords 

mayuscula :: String -> String
mayuscula []     = []
mayuscula (x:xs) = toUpper x : [toUpper x | x <- xs]

-- customFilter :: ([[Char]] -> Bool) -> [a] -> [a]
-- customFilter charInWord char = charInWord == char

filterByToros ::  [([Char],Int)] -> Int -> Int -> [[[Char]]] -> [[[Char]]]
filterByToros listOfTuples num sizeOfList listOfWords = do
   
    if num < sizeOfList  then do
        
        let element = listOfTuples !! num
        
        let char =  fst  element
        let position =  snd  element
        
        let filterList = filter (\x -> x !! position == char) listOfWords
       
        
        filterByToros listOfTuples (num+1) sizeOfList filterList 
       
    else do
       listOfWords

filterByVacas ::  [([Char],Int)] -> Int -> Int -> [[[Char]]] ->  [[[Char]]]
filterByVacas listOfTuples num sizeOfList listOfWords = do
   
    if num < sizeOfList  then do
        
        let element = listOfTuples !! num
        
        let char =  fst  element
        let position =  snd  element
        let filterListContains = filter (\x -> contains char x) listOfWords
        
   
        let filterList = filter (\x -> x !! position /= char ) filterListContains
       
        
        filterByVacas listOfTuples (num+1) sizeOfList filterList 
       
    else do
      listOfWords

    
asignarPuntaje :: [Char] -> Float
asignarPuntaje letra = do
    if (letra == ['A']) || (letra == ['E'])
        then do
            0.1
    else if (letra == ['I']) || (letra == ['N']) || (letra == ['O']) || (letra == ['R']) || (letra == ['S'])
        then 0.2
    else if (letra == ['D']) || (letra == ['L'] )|| (letra == ['C']) || (letra == ['T']) || (letra == ['U'])
        then 0.3
    else if (letra == ['B']) || (letra == ['G']) || (letra == ['M']) || (letra == ['P'])
        then 0.5
    else if (letra == ['F']) || (letra == ['H']) || (letra == ['Q']) || (letra == ['V']) || (letra == ['Y'])
        then 0.8
    else do 1.0

obtenerPuntajeW :: Int -> [[Char]] -> ([[Char]],Float) -> ([[Char]],Float)
obtenerPuntajeW pos palabra puntaje = do 
    if pos < 5
        then do 
            let oldPuntaje = snd puntaje
            let newPuntaje = (palabra, oldPuntaje + asignarPuntaje (palabra !! pos))
            obtenerPuntajeW (pos + 1) palabra newPuntaje
    else do
        puntaje

obtenerPuntajeTot :: Int -> Int -> [[[Char]]] -> [([[Char]],Float)] -> [([[Char]],Float)]
obtenerPuntajeTot pos size listaPalabras listaPuntajes = do
    if pos < size 
        then do 
            let obtener = obtenerPuntajeW 0 (listaPalabras !! pos) (listaPalabras !! pos, 0.0)
            let newLista = listaPuntajes ++ [obtener]
            obtenerPuntajeTot (pos + 1) size listaPalabras newLista
    else do
        listaPuntajes


revisarToros :: Int -> [[Char]] -> [[Char]] -> [([Char],Int)] -> [([Char],Int)]
revisarToros num randomWord eval listOfTuples = do
    if num < 5 
        then do 
            let t = eval !! num
            if t == "T"
                then do 
                    let tupla = (randomWord !! num, num)
                    let newList = listOfTuples ++ [tupla]
                    revisarToros (num+1) randomWord eval newList
                else do revisarToros (num+1) randomWord eval listOfTuples
    else do 
        listOfTuples

revisarVacas :: Int -> [[Char]] -> [[Char]] -> [([Char],Int)] -> [([Char],Int)]
revisarVacas num randomWord eval listOfTuples = do
    if num < 5 
        then do 
            let t = eval !! num
            if t == "V"
                then do 
                    let tupla = (randomWord !! num, num)
                    let newList = listOfTuples ++ [tupla]
                    revisarVacas (num+1) randomWord eval newList
                else do revisarVacas (num+1) randomWord eval listOfTuples
    else do 
        listOfTuples

{--
    Funcion que recibe la palabra ingresada por el usuarios
    la lista de letras que tiene T ancladas en el string de evaluacion
    y el string de evaluacion de la palabra ingresada por el usuario y 
    verifica que no existan V si existe una T anclada a la letra en otro 
    lado de la palabra.Si existe un caso como el anterior, entonces 
    divide el string, cambia la V por - y vuelve a unirla, se llama recursivamente.
--}

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
    

{-- 
    Funcion que recibe la palabra ingresada por el usuario,
    la palabra random generada por la computadora, una lista vacia
    que sera la contenedora del string de evaluacion y otra
    lista contenedora que sera de la letras que tengan T ancladas
    a ellas en el string de evalacion.
--}
    
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


{--
    Funcion encarga de inicializar el modo mente maestra del juego de
    vacas y toros, se llama recursivamente hasta que el usuario ingrese
    una palabra valida para poder se evaluada y se detiene luego del turno
    6, indicandole al usuario que ha perdida y cual era la palabra random.

--}       

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
            initMenteMaestra (currentTurn + 1 ) randomWord


initDecifrador :: Int -> String -> [String] -> Int -> IO ()
initDecifrador currentTurn randomWord listOfWords sizeOfListOfWords = do
    if currentTurn > 6 then putStrLn ("Haz perdido , la palabra era " ++ randomWord )

    else do
        print randomWord
        putStrLn "Evaluacion: "
        x <- getLine
        let x1 = splitOn "" x
        let lchar = drop 1 x1
        let lrW = drop 1 (splitOn "" randomWord)

        let listOfToros =  revisarToros 0 lrW lchar []
        let sizeOfList = length listOfToros

        let listOfVacas =  revisarVacas 0 lrW lchar []
        let sizeOfListVacas = length listOfVacas

        print listOfVacas



        -- print sizeOfList
        let splitedWords = splitAllWords listOfWords 0 sizeOfListOfWords [[[]]]
        let splitAllWordsDroped = drop 1 splitedWords
        -- print splitedWords
        let filteredList = filterByToros listOfToros 0 sizeOfList  splitAllWordsDroped
        print ("FUNCINA CO;O")
        let filteredListVacas = filterByVacas listOfVacas 0 sizeOfListVacas filteredList
        print (filteredListVacas)
        let tamañoPenultima = length filteredListVacas 
        let puntajesAntes = obtenerPuntajeTot 0 tamañoPenultima filteredListVacas []
        print (puntajesAntes)
        let ordenadas = sortOn snd puntajesAntes
        print (ordenadas)
        let tenWords = take 10 ordenadas
        print (tenWords)
        initDecifrador ( currentTurn + 1 ) randomWord listOfWords sizeOfListOfWords






    





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

  
    

    -- Lista de puntos de las palabras
    let word = "12345"
    let ver = "TTTTT"
    let cont = 0

    let randomWordSplited = splitOn "" randomWord
    print randomWordSplited
    let listOfChars = [randomWordSplited, ["1", "2", "3", "4", "5"]]
    print listOfChars
    print (listOfChars !! 1)
    
    initDecifrador 1 randomWord listOfWords sizeOfList
    

    -- initMenteMaestra 1 randomWord

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

    