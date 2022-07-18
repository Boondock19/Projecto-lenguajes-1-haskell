{-|
Module      : Main
Description : Modulo que el implementa el juego Wordle
Copyright   : (c) Ana Sofia Santos, 2022
                  Jose Matias Gonzalez V, 2022
License     : GPL-3
Maintainer  : 1510627@usb.ve 1510627@usb.ve
Stability   : experimental
Portability : POSIX

Programa que implementa el juego Wordle. Contiene dos modos de juego: 
 
    *Modo Mente Maestra: 
    El jugador tiene que adivinar la palabra que ha sido seleccionada por el sistema.
 
    *Modo Descrifrador:
    La computadora debe adivinar la palabra que el jugador selecciona.
-}


module Main where
import Data.List
import System.Environment
import System.Random
import Data.List(sort)
import Data.List.Split
import Data.Char
import Data.Bool



    



-- | Funcion que verifica si un string contiene el caracter especificado.
contains :: Eq a 
    => a 
    -> [a] -- ^ Caracter a buscar.
    -> Bool -- ^ True si el string contiene el caracter especificado , False en caso contrario.
contains elem myList
  = case myList of
      [] -> False
      x : xs | x == elem -> True
      _ : xs -> contains elem xs

{- |
    Funcion que separa todas las palabras de la lista
    grande en caracteres, para poder comparar caracter
    por caracter.
-}

splitAllWords :: [String] -- ^ Lista de palabras a separar.
    -> Int -- ^ posicion de la palabra en el array de palabras.
    -> Int -- ^ tamaño de la lista de palabras.
    -> [[[Char]]] -- ^ Lista de palabras separadas.
    -> [[[Char]]] -- ^ Retorna la lista de palabras separadas.
splitAllWords allWords position size splitWords = do
    if position == size then do
        splitWords
    else do
        let word = allWords !! position
        let x3 = splitOn "" word
        let lchar = drop 1 x3
        let newSplitWords = splitWords ++ [lchar]
        splitAllWords allWords (position + 1) size newSplitWords 

{-|
    Funcion que transforma todas las letras de un string
    en mayusculas.
-}
mayuscula :: String -- ^ String a transformar.
    -> String -- ^ Retorna el string transformado todo en upperCase.
mayuscula []     = []
mayuscula (x:xs) = toUpper x : [toUpper x | x <- xs]



{-|
    Funcion que filtra la lista de todas las palabras, por
    aquellas que posean las letras (Aquellas con T) que 
    componen al String de evaluacion, en la misma posicion . 
    Y retorna un array con las palabras que cumplen con la condicion.
-}

filterByToros ::  [([Char],Int)] -- ^ Array de tuplas con las letras que son T y su posicion
    -> Int  -- ^ Posicion del Array de palabras
    -> Int  -- ^ Tamaño del Array de palabras
    -> [[[Char]]] -- ^ Lista de palabras filtradas por contener T en el string de Evaluacion
    -> [[[Char]]]   -- ^ Retorna la lista de palabras filtradas por contener T en el string de Evaluacion
filterByToros listOfTuples num sizeOfList listOfWords = do
   
    if num < sizeOfList  then do
        
        let element = listOfTuples !! num
        
        let char =  fst  element
        let position =  snd  element
        
        let filterList = filter (\x -> x !! position == char) listOfWords
       
        
        filterByToros listOfTuples (num+1) sizeOfList filterList 
       
    else do
       listOfWords

{-|
    Funcion que filtra la lista de todas las palabras previamente 
    filtradas T, poraquellas que posean las letras (Aquellas con V) que 
    componen al String de evaluacion, en diferente posicion . 
    Y retorna un array con las palabras que cumplen con la condicion.
-}


filterByVacas ::  [([Char],Int)] -- ^ Array de tuplas con las letras que son V y su posicion
    -> Int  -- ^ Posicion del Array de palabras
    -> Int  -- ^ Tamaño del Array de palabras
    -> [[[Char]]] -- ^ Lista de palabras filtradas por contener V en el string de Evaluacion
    ->  [[[Char]]]  -- ^ Retorna la lista de palabras filtradas por contener V en el string de Evaluacion
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

{-|
    Funcion que retorna el valor de evaluacion segun la
    ponderacion especificada en el PDF a partir de una letra. 
-}


asignarPuntaje :: [Char] -- ^ Letra a evaluar
    -> Float -- ^ Retorna el valor de evaluacion de la letra
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


{-|
    Funcion que retorna una tupla con un array de Chars
    y su respectiva evaluacion segun el puntaje. 
-}

obtenerPuntajeW :: Int -- ^ Posicion del array del string
    -> [[Char]] -- ^ String a evaluar caracter por caracter
    -> ([[Char]],Float) -- ^ Tupla con el array de caracteres y su evaluacion
    -> ([[Char]],Float) -- ^ Retorna la tupla con el string y su evaluacion
obtenerPuntajeW pos palabra puntaje = do 
    if pos < 5
        then do 
            let oldPuntaje = snd puntaje
            let newPuntaje = (palabra, oldPuntaje + asignarPuntaje (palabra !! pos))
            obtenerPuntajeW (pos + 1) palabra newPuntaje
    else do
        puntaje

{-|
    Funcion que retorna una lista de tuplas con arrays de Chars
    y su respectiva evaluacion segun el puntaje. 
-}

obtenerPuntajeTot :: Int -- ^ Posicion del array del string
    -> Int -- ^ Tamaño del array de palabras
    -> [[[Char]]] -- ^ Array de palabras a evaluar
    -> [([[Char]],Float)] -- ^  Array de tuplas con el string y su evaluacion
    -> [([[Char]],Float)] -- ^ Retorna el array de tuplas con el string y su evaluacion
obtenerPuntajeTot pos size listaPalabras listaPuntajes = do
    if pos < size 
        then do 
            let obtener = obtenerPuntajeW 0 (listaPalabras !! pos) (listaPalabras !! pos, 0.0)
            let newLista = listaPuntajes ++ [obtener]
            obtenerPuntajeTot (pos + 1) size listaPalabras newLista
    else do
        listaPuntajes


{-|
    Funcion que toma el string de evaluacion ingresado por el usuario,
    la palabra adivinada por la computadora y un array vacio de tuplas
    y retorna un array de tuplas con el caracter correspondiente a una T
    junto a la posicion de la misma.
-}

revisarToros :: Int  -- ^ Posicion del array del string.
    -> [[Char]] -- ^ String a evaluar caracter por caracter (la palabra adivinada por la computadora).
    -> [[Char]] -- ^ String de evaluacion ingresada por el usuario.
    -> [([Char],Int)] -- ^  Array de tuplas con el caracter correspondiente a una T y su posicion en la palabra adivinada por la computadora.
    -> [([Char],Int)] -- ^ Retorna un array de tuplas con el caracter correspondiente a una T y su posicion en la palabra de evaluacion ingresada por el usuario.
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


{-|
    Funcion que toma el string de evaluacion ingresado por el usuario,
    la palabra adivinada por la computadora y un array vacio de tuplas
    y retorna un array de tuplas con el caracter correspondiente a una V
    junto a la posicion de la misma.
-}

revisarVacas :: Int -- ^ Posicion del array del string.
    -> [[Char]] -- ^ String a evaluar caracter por caracter (la palabra adivinada por la computadora).
    -> [[Char]] -- ^ String de evaluacion ingresada por el usuario.
    -> [([Char],Int)] -- ^  Array de tuplas con el caracter correspondiente a una V y su posicion en la palabra adivinada por la computadora.
    -> [([Char],Int)] -- ^ Retorna un array de tuplas con el caracter correspondiente a una V y su posicion en la palabra de evaluacion ingresada por el usuario.
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

{-|
    Funcion que recibe la palabra ingresada por el usuarios
    la lista de letras que tiene T ancladas en el string de evaluacion
    y el string de evaluacion de la palabra ingresada por el usuario y 
    verifica que no existan V si existe una T anclada a la letra en otro 
    lado de la palabra.Si existe un caso como el anterior, entonces 
    divide el string, cambia la V por - y vuelve a unirla, se llama recursivamente.
--}

revisionV:: Int -- ^ Posicion del array del string
    -> [[Char]]  -- ^ String a evaluar caracter por caracter (la palabra ingresada por el usuario)
    -> [[Char]]  -- ^ Array de T de la palabra ingresada por el usuario
    -> [Char] -- ^ String de evaluacion de la palabra ingresada por el usuario
    -> [Char] -- ^ String de evaluacion de la palabra ingresada por el usuario con las V cambiadas por -
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
    

{-|  
    Funcion que recibe la palabra ingresada por el usuario,
    la palabra random generada por la computadora, una lista vacia
    que sera la contenedora del string de evaluacion y otra
    lista contenedora que sera de la letras que tengan T ancladas
    a ellas en el string de evalacion.
-}
    
revisar :: Int -- ^ Posicion del array del string.
    -> [[Char]] -- ^ Palabra ingresada por el usuario al adivinar.
    -> [[Char]]  -- ^ Palabra random generada por la computadora, que es la palabra a adivinar por el usuario.
    -> [Char] -- ^ String de evaluacion de la palabra ingresada por el usuario.
    -> [[Char]] -- ^ Letras que el usuario acerto y por lo tanto seran T del string de evaluacion.
    -> [Char] -- Llamada a revisionV para revisar si existen V repetidas en el string de evaluacion.
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

eliminarPalabras :: Int --indice
    -> Int --longitud de lista de todas las palabras
    -> [[[Char]]] --lista a verificar
    -> [[[Char]]] --lista de palabras nueva
    -> [[[Char]]] --lista de todas las palabras
    -> [[[Char]]] --devolver lista filtrada
eliminarPalabras indice long listaUno listaDos listaTodas = do
    if long > indice 
        then do 
            let word = listaTodas !! indice
            if contains word listaUno
                then do 
                    eliminarPalabras (indice+1) long listaUno listaDos listaTodas

            else do
                let new = listaDos ++ [word]
                eliminarPalabras (indice+1) long listaUno new listaTodas
    else do
        listaDos

listarSinCosto :: Int --indice
    -> Int --longitud
    -> [([[Char]],Float)] -- lista con costo
    -> [[[Char]]] --lista sin costo
    -> [[[Char]]] --devolver lista sin costo
listarSinCosto indice long listCost listWithout = do
    if indice < long
        then do
            let pair = listCost !! indice
            let word = fst pair
            let new = listWithout ++ [word]
            listarSinCosto (indice+1) long listCost new
    else do 
        listWithout

asignarEval :: [Char] --letra de la evaluacion
    -> Float --valor
asignarEval letra = do
    if letra == ['T']
        then do
            0.2
    else if letra == ['V']
        then 0.1
    else do
        0.0

obtenerPuntajeE :: Int -- ^ Posicion del array del string
    -> [[Char]] -- ^ String a evaluar caracter por caracter
    -> [[Char]] -- ^ Palabra del diccionario
    -> ([[Char]],Float) -- ^ Tupla con el array de caracteres y su evaluacion
    -> ([[Char]],Float) -- ^ Retorna la tupla con el string y su evaluacion
obtenerPuntajeE pos eval palabra puntaje = do 
    if pos < 5
        then do 
            let oldPuntaje = snd puntaje
            let newPuntaje = (palabra, oldPuntaje - asignarEval (eval !! pos))
            obtenerPuntajeE (pos + 1) eval palabra newPuntaje
    else do
        puntaje

empezarEval :: [[Char]] --String de evaluacion creado
    -> [[Char]] --Palabra asociada
    -> ([[Char]],Float) -- ^ Retorna la tupla con el string y su evaluacion
empezarEval eval palabra = do
    let par = obtenerPuntajeE 0 eval palabra (eval,1.0)
    par


posibilidad :: [Char] -> Int -> [String]
posibilidad chars 1 = map (:[]) chars
posibilidad chars n = concatMap (\front -> map (front ++) (posibilidad chars 1)) $ posibilidad chars (n - 1)
    
todasPosibilidades :: [String]
todasPosibilidades = concatMap (posibilidad (['T','V','-'])) [5]

limpiarUnaXUna :: Int -- indice
    -> [[Char]] -- evaluacion
    -> String -- posibilidad a comparar
    -> [String] --lista con todas las posibilidades
    -> [[Char]] --lista a llenar
    -> [[Char]] --devolver la posibilidad
limpiarUnaXUna indice eval posibility todas filtro = do
    
    if indice < 5
        then do
            let letraE = eval !! indice
            let letraP = posibility !! indice
            if letraE == "T"
                then do 
                    if letraP == 'T'
                        then do
                            let new = filtro ++ [[letraP]]
                            limpiarUnaXUna (indice+1) eval posibility todas new
                            
                    else do
                        limpiarUnaXUna (indice+1) eval posibility todas filtro
            else do
                let new = filtro ++ [letraE]
                limpiarUnaXUna (indice+1) eval posibility todas new            

    else do 
        filtro 


limpiarEvals :: Int --indice
    -> Int --longitud
    -> [[Char]] --evaluacion del usuario
    -> [String] --lista con todas las posibilidades
    -> [String] --lista a llenar
    -> [String] --devuelve la lista limpia
limpiarEvals indice long eval todas filtro = do
    if indice < long
        then do 
            let posibility = todas !! indice
            let newWord = limpiarUnaXUna 0 eval posibility todas []
            let size = length newWord
            if size == 5
                then do
                    let newList = filtro ++ newWord
                    limpiarEvals (indice+1) long eval todas newList
            else do
                limpiarEvals (indice+1) long eval todas filtro
    else do
        filtro

-- createEvaluationStringTV :: Int -> [[Char]] -> [[[Char]]] ->[[[Char]]]
-- createEvaluationStringTV counter evaluationString stringEvaluationList = do
--     if counter < 5
--         then do
--             let (x,_:ys) = splitAt counter evaluationString
--             let stringArreglado = [x ++ ["T"] ++ ys]
--             let newEvaluationStringList = stringEvaluationList ++ stringArreglado
--             createEvaluationStringTV (counter+1) (head stringArreglado) newEvaluationStringList
--     else do
--         stringEvaluationList

-- {- |
--     Funcion que recibe un counter que representa el index de
--     las palabras, el string de evaluacion generado y una lista 
--     contenedora de los strings . 

-- -}

-- createEvaluationStringFromBeginningV :: Int -> [[Char]] -> [[[Char]]] -> [[[Char]]]
-- createEvaluationStringFromBeginningV counter evaluationString stringEvaluationList = do
--     if counter < 5
--         then do
            
--             let (x,_:ys) = splitAt counter evaluationString
--             let stringArreglado = [x ++ ["V"] ++ ys]
--             let newEvaluationStringList = stringEvaluationList ++ stringArreglado
--             createEvaluationStringFromBeginningV (counter+1) (head stringArreglado) newEvaluationStringList
--     else do
--         stringEvaluationList

-- createEvaluationStringFromBackT :: Int -> [[Char]] -> [[[Char]]] -> [[[Char]]]
-- createEvaluationStringFromBackT counter evaluationString stringEvaluationList = do
--     if counter > 0
--         then do
            
--             let (x,_:ys) = splitAt counter evaluationString
--             let stringArreglado = [x ++ ["T"] ++ ys]
--             let newEvaluationStringList = stringEvaluationList ++ stringArreglado
--             createEvaluationStringFromBackT (counter-1) (head stringArreglado) newEvaluationStringList
--     else do
--         stringEvaluationList

{- |
    Funcion que recibe un counter que representa el index de
    las palabras, el string de evaluacion generado y una lista 
    contenedora de los strings . 

-}

-- createEvaluationStringFromBackV :: Int -> [[Char]] -> [[[Char]]] ->IO()
-- createEvaluationStringFromBackV counter evaluationString stringEvaluationList = do
--     if counter > 0
--         then do
            
--             let (x:y,_) = splitAt counter evaluationString
--             print ("Este es x: " ++ show x)
--             print ("Este es ys: " ++ show y)
--             let stringArreglado = [x ++ ["V"] ++  y]
--             let newEvaluationStringList = stringEvaluationList ++ stringArreglado
--             createEvaluationStringFromBackV (counter-1) (head stringArreglado) newEvaluationStringList
--     else do
--         print stringEvaluationList

{- |
    Funcion que recibe un String o [Char] (El string de evaluacion)
    y un String o [Char] (La palabra generada/escogida).
    Y retorna una lista de [[[Char]]] (La lista de todas las posibilidades validas)
    A partir de un string de evaluacion, la palabra escogida y la lista de 
    todas las palabras.
-}
getFilteredValidWords :: String -- ^ string de evaluacion
    -> String -- ^ string escogido
    -> [[[Char]]] -- ^ lista de todas las palabras separadas
    -> [[[Char]]] -- ^ lista de todas las palabras validas obtenidas
getFilteredValidWords evaluationString randomWord  splitAllWordsDroped   = do
        let x1 = splitOn "" evaluationString
        let lchar = drop 1 x1
        let lrW = drop 1 (splitOn "" randomWord)

        let listOfToros =  revisarToros 0 lrW lchar []
        let sizeOfList = length listOfToros

        let listOfVacas =  revisarVacas 0 lrW lchar []
        let sizeOfListVacas = length listOfVacas

        
        let sizeSplitsAll = length splitAllWordsDroped
        -- print splitedWords
        let filteredList = filterByToros listOfToros 0 sizeOfList  splitAllWordsDroped
        let filteredListVacas = filterByVacas listOfVacas 0 sizeOfListVacas filteredList
        filteredListVacas

{- |
    Funcion que recibe un counter que representa el index de
    los strings de evaluacion palabras, la palabra obtenida y una lista 
    contenedora de los strings validos de evaluacion . 
    
-}

createListOfValidWords :: Int -- ^ counter
    -> Int -- ^ tamaño de la lista de strings de evaluacion
    -> [String] -- ^ array de strings de evaluacion
    -> String -- ^ string escogido
    -> [[[Char]]] -- ^ lista de todas las palabras separadas
    -> [String] -- ^ lista de todas las palabras validas obtenidas
    -> [String] -- ^ return lista de todas las palabras validas obtenidas
createListOfValidWords counter sizeOfEvaluationList allEvaluationStrings chosenWord splitAllWordsDroped currentList = do
    if counter < sizeOfEvaluationList
        then do
            let validWords = getFilteredValidWords (allEvaluationStrings !! counter) chosenWord splitAllWordsDroped
            let isValid = length validWords > 0  
            if isValid
                then do
                    let newList = currentList ++ [allEvaluationStrings !! counter]
                    createListOfValidWords (counter+1) sizeOfEvaluationList allEvaluationStrings chosenWord splitAllWordsDroped newList
                else do
                    createListOfValidWords (counter+1) sizeOfEvaluationList allEvaluationStrings chosenWord splitAllWordsDroped currentList
        else do
            currentList

{-|
    Funcion encarga de inicializar el modo mente maestra del juego de
    vacas y toros, se llama recursivamente hasta que el usuario ingrese
    una palabra valida para poder se evaluada y se detiene luego del turno
    6, indicandole al usuario que ha perdidO y cual era la palabra random.
    En caso de que el usuario adivine correctamente se le indica que ha ganado.

-}       

initMenteMaestra :: Int -- ^ Turno actual del juego.
    -> String -- ^ Palabra random escogida por la computadora.
    -> IO ()
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

{-
    Funcion encarga de inicializar el modo descrifrador del juego de
    vacas y toros, se llama recursivamente hasta que el usuario ingrese
    un string de evaluacion TTTTT e indicar que la computadora ha ganado.
    En caso contrario al pasar los 6 turnos se le indica que ha perdido la computadora.

-}  

initDecifrador :: Int -- ^ Turno actual del juego.
    -> String -- ^ Palabra random escogida por la computadora.
    -> [String] -- ^ Lista de todas las palabras.
    -> [[[Char]]] -- ^ Lista de todas las palabras separadas.
    -> Int  -- ^ Tamaño de la lista de palabras.
    -> IO ()
initDecifrador currentTurn randomWord listOfWords splitAllWordsDroped sizeOfListOfWords = do
    if currentTurn > 6 then putStrLn ("Haz perdido , la palabra era " ++ randomWord )

    else do
        print randomWord
        putStrLn "Evaluacion: "
        x <- getLine
        print ("Evaluacion: " ++ x)
        let x1 = splitOn "" x
        let lchar = drop 1 x1
        let lrW = drop 1 (splitOn "" randomWord)

        let listOfToros =  revisarToros 0 lrW lchar []
        let sizeOfList = length listOfToros

        let listOfVacas =  revisarVacas 0 lrW lchar []
        let sizeOfListVacas = length listOfVacas

       
        let sizeSplitsAll = length splitAllWordsDroped
        -- print splitedWords
        let filteredList = filterByToros listOfToros 0 sizeOfList  splitAllWordsDroped
        let filteredListVacas = filterByVacas listOfVacas 0 sizeOfListVacas filteredList

        print (filteredListVacas)
       
        let tamañoPenultima = length filteredListVacas 
        let puntajesAntes = obtenerPuntajeTot 0 tamañoPenultima filteredListVacas []
        
        let ordenadas = sortOn snd puntajesAntes
       
        let tenWords = take 10 ordenadas
        let firstWord = head tenWords
        
        --print (tenWords)

        {-
            Aqui si la lista de 10 palabras esta vacia, quiere decir que no hay ninguna palabra que cumpla con los requisitos
            y por lo tanto el usuario esta haciendo trampa.
        -}

        -- let nivel1 = tenWords !! 0
        -- let palabra1 = fst nivel1
        -- --print (palabra1)
        -- let sizeTenWords = length tenWords
        -- let listaSinCosto = listarSinCosto 0 sizeTenWords tenWords []
        -- --print(listaSinCosto)
        -- let todas = eliminarPalabras 0 sizeSplitsAll listaSinCosto [] splitAllWordsDroped
        -- let ver = contains palabra1 todas
        -- --print (ver)
        -- let ver2 = empezarEval lchar lrW 
        --print(ver2)
        --let evaluationStrings = createEvaluationStringTV 0 ["-","-","-","-","-"] []
        --print (evaluationStrings)

        let var = todasPosibilidades
        let sizeEvals = length var
        print  sizeEvals
        let limpieza = limpiarEvals 0 sizeEvals lchar var []
        print var
        let palabraElegida = concat (fst firstWord)
        print palabraElegida
        print(var !! 17)
        let filteredListVacas2 = getFilteredValidWords (var !! 17) (concat (fst firstWord)) splitAllWordsDroped
        print filteredListVacas2
        let allPossiblesEvaluationStrings = createListOfValidWords 0 sizeEvals var palabraElegida splitAllWordsDroped []
        putStrLn "Salida de todos los posibles strings de evaluacion: "
        print allPossiblesEvaluationStrings
        initDecifrador ( currentTurn + 1 ) randomWord listOfWords splitAllWordsDroped sizeOfListOfWords










main = do
    let program = ""
    program <- getArgs
    fileContent <- readFile "Palabras.txt"
    -- Lista de todas las palabras.
    

    let listOfWords = lines fileContent
    let sizeOfList = length listOfWords

    -- Obtenemos una palabra aleatoria de la lista.

    index <- randomRIO (0, sizeOfList - 1)
    let randomWord =  listOfWords !! index


    let programLenght = length program

    if programLenght > 0 then do
        print ("La palabra es : " ++ randomWord ++ " con index : " ++ show index)
        if program !! 0 == "mentemaestra" then do
            initMenteMaestra 0 randomWord
        else if program !! 0 == "descifrador" then do

            {-
                Hacer drop de la palabra adivinada por la computadora 
                y de igual manera en las llamadas recursivas.
                
            -}
            let splitedWords = splitAllWords listOfWords 0 sizeOfList [[[]]]
            let splitAllWordsDroped = drop 1 splitedWords
            initDecifrador 0 randomWord listOfWords splitAllWordsDroped sizeOfList
        else do
            putStrLn "No se reconoce el programa"
    else do
        putStrLn "No Ingreso ningun argumento al programa"
            
    

    