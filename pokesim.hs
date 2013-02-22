-- Nombre del archivo: pokesim.hs

module Main (
main ) where

--import Pokemon
import System.IO
import System.Environment
import Data.List.Split
import Pokemon

	            

--menu :: fIRMA POR DEFINIR 
menu especies ataques entrenador1 entrenador2 turno rendicion1 rendicion2 = 
 case turno of
   "0" ->
    do
      putStrLn "Por favor Entrenador 1 introduzca un comando: "
      comando <- getLine
      case comando of
        "info yo" ->  
          if (rendicion2 == 1) then
            do
             {
              putStrLn "VICTORIA PARA EL ENTRENADOR 1"
              putStrLn "FIN DE LA PARTIDA"
             }
          else
            do
             {
               putStrLn " Aqui llamar a la funcion que muestra el catalogo PROPIO"
               menu especies ataques entrenador1 entrenador2 0 0 0
             }
        "info rival" ->
          if (rendicion2 == 1) 
          then
            do
             {
               putStrLn "VICTORIA PARA EL ENTRENADOR 1"
               putStrLn "FIN DE LA PARTIDA"
             }
          else 
            do
             {
              putStrLn "Aqui llamar a la funcion que muestra el catalogo del RIVAL"
              menu especies ataques entrenador1 entrenador2 0 0 0
             }
        "ayuda" ->
          if (rendicion2 == 1) then
            do
             {
              putStrLn "VICTORIA PARA EL ENTRENADOR 1"
              putStrLn "FIN DE LA PARTIDA"
             }
          else
           do
            {
             putStrLn "Mostrar Lista de Ataques (con el numero de comando pertinente)"
             putStrLn "Mostrar cuantos PPs quedan en el ataque"
             putStrLn "Listar los monstruos que quedan"
             menu especies ataques entrenador1 entrenador2 0 0 0
            }
        _ -> 
          if ((take 6 comando)=="atacar") 
          then 
            if (rendicion2 == 1) then do {
                           putStrLn "VICTORIA PARA EL ENTRENADOR 1"
                           putStrLn "FIN DE LA PARTIDA"
                           }
            else do 
                {
                 putStrLn "Realizar ataque"
                 menu especies ataques entrenador1 entrenador2 1 0 0
                }
          else
             if ((take 7 comando) == "cambiar")
             then 
               if (rendicion2 == 1) 
               then
                 do
                   {
                     putStrLn "VICTORIA PARA EL ENTRENADOR 1"
                     putStrLn "FIN DE LA PARTIDA"
                   }
               else
                 do
                   {
                    putStrLn "Realizar Cambios en entrenador 1"
                    menu especies ataques entrenador1 entrenador2 1 0 0
                   }
             else
              if (comando == "rendirse") 
              then 
               if (rendicion2 == 1)
               then
                 do
                   {
                     putStrLn "EMPATE"
                     putStrLn "FIN DE LA PARTIDA"
                   }
               else 
                do
                  {
                    menu especies ataques entrenador1 entrenador2 1 1 0
                  }
              else 
                do
                 {
                   putStrLn "EMPATE"
                   putStrLn "FIN DE LA PARTIDA"
                 }
   "1" ->
    do
      putStrLn "Por favor Entrenador 2 introduzca un comando: "
      comando <- getLine
      case comando of
        "info yo" ->
          if (rendicion1 == 1)
          then
            do
              {
               putStrLn "VICTORIA PARA EL ENTRENADOR 2"
               putStrLn "FIN DE LA PARTIDA"
              }
          else
           do
            {
             putStrLn " Aqui llamar a la funcion que muestra el catalogo PROPIO"
             menu especies ataques entrenador1 entrenador2 0 0 0
            }
        "info rival" ->
          if (rendicion1 == 1)
          then
            do
              {
                putStrLn "VICTORIA PARA EL ENTRENADOR 2"
                putStrLn "FIN DE LA PARTIDA"
              }
          else
            do
              {            
               putStrLn "Aqui llamar a la funcion que muestra el catalogo del RIVAL"
               menu especies ataques entrenador1 entrenador2 0 0 0
              }
        "ayuda"     ->
          if (rendicion1 == 1)
          then
            do
             {
              putStrLn "VICTORIA PARA EL ENTRENADOR 2"
              putStrLn "FIN DE LA PARTIDA"
             }                        
          else
           do
            {
             putStrLn "Mostrar Lista de Ataques (con el numero de comando pertinente)"
             putStrLn "Mostrar cuantos PPs quedan en el ataque"
             putStrLn "Listar los monstruos que quedan"
             menu especies ataques entrenador1 entrenador2 0 0 0
            }
        _ -> do
                if ((take 6 comando)=="atacar")
                then
                  if (rendicion1 == 1)
                  then
                  do
                   {
                     putStrLn "VICTORIA PARA EL ENTRENADOR 2"
                     putStrLn "FIN DE LA PARTIDA"
                   }             
                  else
                   do
                    {
                     putStrLn "Realizar ataque"
                     menu especies ataques entrenador1 entrenador2 1 0 0
                    }
                else
                     if ((take 7 comando) == "cambiar")
                     then
                      do
                       {
                         if (rendicion1 == 1) then do
                                 putStrLn "VICTORIA PARA EL ENTRENADOR 2"
                                 putStrLn "FIN DE LA PARTIDA"
                         else do
                           putStrLn "Realizar Cambios en entrenador 1"
                           menu especies ataques entrenador1 entrenador2 1 0 0
                        }
                     else
                          if (comando == "rendirse")
                          then
                           if (rendicion1 == 1) 
                           then
                            do
                             {
                               putStrLn "EMPATE"
                               putStrLn "FIN DE LA PARTIDA"
                             }
                           else
                            do
                              {
                                menu especies ataques entrenador1 entrenador2 1 0 1
                              }
                          else
                           do
                            {
                             menu especies ataques entrenador1 entrenador2 1 0 1
                            }


main :: IO()        
main = do 
  argumentos <- getArgs
  if (length argumentos/=4) then putStrLn "Error: Falta la direccion de uno de los archivos. Alto" 
                            else do
                                   especies <- readFile (argumentos!!0)     
                                   return ((map (splitOn ",") (lines especies)))     -- Tomarlo en cuenta para crearEspecie. Ya esta la lista de especies.               
       {-                            ataques <- readFile (argumentos!!1)                 
                                   return ( (map (splitOn ",") (lines ataques)))     -- Tomarlo en cuenta para crearAtaque. Ya esta la lista de ataque.       
                                   entrenador1 <- readFile (argumentos!!2)     
                                   return ( (map (splitOn ",") (lines especies)))     -- Tomarlo en cuenta para crearEspecie. Ya esta la lista de especies.               
                                   entrenador2 <- readFile (argumentos!!3)
                                   return ( (map (splitOn ",")(lines especies)))     -- Tomarlo en cuenta para crearEspecie. Ya esta la lista de especies.      
-}  -- TERMINAR        
                                   menu (crearEspecie (map splitOn (lines especies))) (crearAtaques (lines ataques)) 
                                        (crearEntrenador1 (lines entrenador1))
                                        (crearEntrenador2 (lines entrenador2)) 0 0 0 