import Pokemon
import System.IO
import System.Environment
import Data.List.Split

menu :: a -> IO()
menu a = do

    
    putStrLn
      $  "\n "
      ++ "Que quieres hacer? \n"  
      ++ "1) Crear  Nuevo \n"
      ++ "2) Predecir \n"
      ++ "3) Persistir \n"
      ++ "4) Cargar \n"
      ++ "5) Consultar Pregunta Crucial \n"
      ++ "6) Consultar Estadisticas"
    putStr "Su Seleccion [1-6]: "
    hFlush stdout
    seleccion <- getLine 

    case seleccion of 
      "1" -> putStr "\n Su Seleccion [1]: "
      "2" -> putStr "\n Su Seleccion [2]: "
      "3" -> putStr "\n Su Seleccion [3]: "
      "4" -> putStr "\n Su Seleccion [4]: "
      "5" -> putStr "\n Su Seleccion [5]: "
      "6" -> putStr "\n Su Seleccion [6]: "
      _  -> putStr "\n: "
       
       
      
    menu 24
    
    
main :: IO ()
main = do 
    argumentos <- getLine
    let  especies = readFile argumentos 
   -- lines especies
   -- return (map (splitOn ",") (lines especies))     -- Tomarlo en cuenta para crearEspecie. Ya esta la lista de especies.               

    menu 24