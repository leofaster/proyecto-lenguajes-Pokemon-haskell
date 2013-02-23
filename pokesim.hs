-- Nombre del archivo: pokesim.hs



--import Pokemon
import System.IO
import System.Environment
import Data.List.Split
import Pokemon
import Data.Char
-- Busca el Ataque Basado en el numero que te dan de entrada
numeroAtaque :: Monstruo -> Int -> Maybe Ataque
numeroAtaque m a = 
  case a of
    1 -> ataque1 m
    2 -> ataque2 m
    3 -> ataque3 m
    4 -> ataque4 m
    
--Verifica si el ataque existe
veridicarAtaque ::  Monstruo -> Int -> Bool
veridicarAtaque monstruo ataque =
  case (numeroAtaque monstruo ataque) of
    Nothing -> False
    Just  ataque1  ->   
        if (puntoPoder ataque1 > 1)
        then 
          True
        else
          False
  


-- Imprime el menu
imprimirMenu :: Int -> IO()
imprimirMenu a = do
    putStrLn "Opciones a elegir\n "  
    putStrLn " Atacar n \n"
    putStrLn " Cambiar n  \n"
    putStrLn " Rendirse \n"
    putStrLn " Info \n"
    putStrLn " Ayuda \n"
    putStrLn "Su Seleccion: "
  
	--jugador true = jugador 1
-- Funcion que actualiza el Campo de Batalla una vez que estan elegidas las acciones
realizarAccion :: (String,Int) -> (String, Int) -> CampoBatalla -> CampoBatalla
realizarAccion (a,b) (c,d) campoBatalla = campoBatalla
 -- if a = "a"
 -- then
  -- if  c = "a"
  -- then 
    -- if (evaluarVelocidad  (head $ entrenador1 campoBatalla) (head $ entrenador2 campoBatalla) ==1 )
    -- then 
      -- aplicarAtaque 
      -- actualizarEstado
      -- restarPoder
      -- aplicarAtaque
      -- actualizarEstado
      -- restarPoder
      
  -- else
 -- else

-- Funcion para elegir las acciones a realizar
jugada :: CampoBatalla -> Bool -> Int -> Int -> (String,Int) -> (String,Int) -> IO()
jugada campoBatalla jugador at bt (ds,nds) (sd,nsd)= 
 if at == bt 
 then 
  jugada (realizarAccion (ds,nds)(sd,nsd) campoBatalla) jugador 0 0 ("i",0)("i",0)
 else 
 
 

 do
  imprimirMenu 1
  hFlush stdout
  decicion <- getLine

  case (map toLower $  head $  words decicion) of 
    "atacar"   ->
        if (((read  (head  (tail  (words decicion))) :: Int)) >= 3 )
        then
          if jugador 
          then
           if (veridicarAtaque (head $ entrenador1 campoBatalla) 3)
           then
            --putStrLn "Su Seleccion: "
            jugada campoBatalla jugador 1 0 ("a",3) (sd,nsd)
           else
            jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
          else 
            if (veridicarAtaque (head $ entrenador2 campoBatalla) 3)
            then
            -- putStrLn "Su Seleccion: "
             jugada campoBatalla jugador 0 1 (ds,nds) ("a",3)
             -- ("a",3)
            else
             jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
        else
          if jugador
          then
            if (veridicarAtaque (head $ entrenador1 campoBatalla) (((read  (head  (tail  (words decicion))) :: Int))))
            then
             -- putStrLn "Su Seleccion: "
              jugada campoBatalla jugador 1 0 ("a",(read  (head  (tail  (words decicion))) :: Int)) (sd,nsd)
            --  ("a",(tail $  words decicion))
            else
             jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
          else
            if (veridicarAtaque (head $ entrenador2 campoBatalla) ((read  (head  (tail  (words decicion))) :: Int)))  
            then
             -- putStrLn "Su Seleccion: "
              jugada campoBatalla jugador 0 1 (ds,nds) ("a",(read  (head  (tail  (words decicion))) :: Int))
             -- ("a",(tail $  words decicion))
            else
             jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
    "cambiar"  ->
      putStrLn "Su Seleccion: "
       -- ("c",(tail $  words decicion))
    "rendirse" ->
      putStrLn "Su Seleccion: "
       -- ("r",0)
    "info"     -> 
        if jugador
        then 
          case map toLower $  last $  words decicion of
            "yo"    -> do
             imprimirMonstruo (head (entrenador1 campoBatalla))
             jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
            "rival" -> do
              imprimirMonstruo (head  (entrenador2 campoBatalla))
              jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
        else 
          case map toLower $  last $  words decicion of
            "yo"    -> do
              imprimirMonstruo (head $ entrenador2 campoBatalla)
              jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
            "rival" -> do
              imprimirMonstruo (head $ entrenador1 campoBatalla)
              jugada campoBatalla jugador 0 0  (ds,nds) (sd,nsd)
    "ayuda" -> 
        if jugador 
        then do
          imprimirAyudaEntrenador (entrenador1 campoBatalla) (head $ entrenador1 campoBatalla)
          jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
        else do
          imprimirAyudaEntrenador (entrenador2 campoBatalla) (head $ entrenador2 campoBatalla)
          jugada campoBatalla jugador 0 0 (ds,nds) (sd,nsd)
  
  
  
  
  
menu :: CampoBatalla ->  IO()           
menu campoBatalla  = do

  putStr "Su Seleccion: "


main :: IO()        
main = do 
    argumentos <- getArgs
    if (length argumentos /= 4) 
    then
        putStrLn "Error: Falta la direccion de uno de los archivos. Alto" 
    else
      do
       especies <- readFile (argumentos!!0)   
       especiesListo <- return ((map (splitOn ",") (lines especies)))
       
       ataques <- readFile (argumentos!!1) 
       ataquesListo <- return ((map (splitOn ",") (lines ataques)))
       
       
       entrenador1 <- readFile (argumentos!!2) 
       entrenador1Listo <- return ((map (splitOn ",") (lines entrenador1)))
      
       
       entrenador2 <- readFile (argumentos!!3) 
       entrenador2Listo <- return ((map (splitOn ",") (lines entrenador2)))
       
       
       
       --jugada (crearPokedex especiesListo) (crearAtaquedex ataquesListo) (crearEntrenador (crearPokedex especiesListo) (crearAtaquedex ataquesListo) entrenador1Listo ) (crearEntrenador (crearPokedex especiesListo) (crearAtaquedex ataquesListo) entrenador2Listo  ) True 0 0 ("i",0)("i",0) 
       --jugada (crearPokedex especiesListo) (crearAtaquedex ataquesListo) (crearEntrenador (crearPokedex especiesListo) (crearAtaquedex ataquesListo) entrenador1Listo ) (crearEntrenador (crearPokedex especiesListo) (crearAtaquedex ataquesListo) entrenador2Listo  ) False 0 0 ("i",0)("i",0)
       
       
       putStrLn "" 
       

    