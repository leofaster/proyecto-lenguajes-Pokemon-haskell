import System.IO
import System.Environment
import Data.List.Split
import Pokemon


main :: IO()        
main = 
  do
      argumentos <- getLine
      source <- readFile argumentos
      listo <- lines source
      return listo
      putStr listo
      
      
    --  especies <- readFile (argumentos)     
    --  listo <-((map (splitOn ",") (lines especies)))
    --  return listo