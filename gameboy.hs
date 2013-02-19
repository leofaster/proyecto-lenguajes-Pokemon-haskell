import Pokemon
import System.IO

menu :: a -> IO()
menu a = do
    -- print oraculo
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
    --seleccion <- getLine 


main :: IO ()
main = menu 24 