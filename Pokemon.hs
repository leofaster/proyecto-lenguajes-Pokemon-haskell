module Pokemon
  (Tipo (Bug, Dark, Dragon, Electric, Fighting, Fire, Flying, Ghost, Grass, Ground, Ice, Normal, Poison, Psychic, Rock, Steel, Water)
  , CampoBatalla (..)
  , Especie (..)
  , Ataque (..)
  , Monstruo (..)
  , crearPokedex
  , crearAtaquedex
  , buscarEspecie
  , imprimirEspecie
  , cambiarMonstruo
  , imprimirMonstruo
  , buscarAtaque
  , aplicarAtaque
  , evaluarVelocidad
  , crearEntrenador
  , imprimirAyudaEntrenador
  , crearCampoBatalla
  , imprimirCampoBatalla
  , actualizarEstado
  )
where

import Data.List
import Data.Char
import Data.Maybe

data CampoBatalla
   = CampoBatalla
    { pokedex :: [Especie]
    , ataquedex :: [Ataque] 
    , entrenador1 :: [Monstruo]
    , entrenador2 :: [Monstruo]
    }
  deriving Show

data Tipo
  = Bug
  | Dark
  | Dragon
  | Electric
  | Fighting
  | Fire
  | Flying
  | Ghost
  | Grass
  | Ground
  | Ice
  | Normal
  | Poison
  | Psychic
  | Rock
  | Steel
  | Water
  deriving (Bounded, Eq, Enum, Read, Show)
  
data Especie 
  = Especie
    { numero :: Int 
    , nombreEspecie :: String
    , tipoEspecie :: Either Tipo (Tipo,Tipo)
    , hpEspecie :: Int 
    , ataque :: Int
    , defensa :: Int
    , ataqueEspecial :: Int
    , defensaEspecial :: Int
    , velocidad :: Int
    , preEvolucion :: Int
    , evolucion :: String
    }
  deriving (Read, Show)
  
data Ataque
  = Ataque
    { nombreAtaque :: String
    , tipoAtaque :: Tipo 
    , forma :: Bool
    , puntoPoder :: Int 
    , poder :: Int 
    }
  deriving (Eq, Read, Show)
  
data Monstruo
  = Monstruo
    { especie :: Especie
    , sobreNombre :: String
    , nivel :: Int
    , hp :: Int
    , ataqueM :: Int
    , defensaM :: Int
    , ataqueEM :: Int
    , defensaEM :: Int
    , velocidadM :: Int
    , ataque1 :: Maybe Ataque 
    , ataque2 :: Maybe Ataque
    , ataque3 :: Maybe Ataque
    , ataque4 :: Maybe Ataque
    }
  deriving (Read, Show)

-- Crea una Especie a partir de una lista de Strings con sus datos.
crearEspecie :: [String] -> Especie
crearEspecie datos
   | null $ datos!!3 = Especie {numero = read $ datos!!0::Int, 
                              nombreEspecie = datos!!1, 
                              tipoEspecie = read $ datos!!2::Either Tipo (Tipo,Tipo), 
                              hpEspecie = read $ datos!!4::Int,
                              ataque = read $ datos!!5::Int, 
                              defensa = read $ datos!!6::Int,
                              ataqueEspecial = read $ datos!!7::Int, 
                              defensaEspecial = read $ datos!!8::Int, 
                              velocidad = read $ datos!!9::Int,
                              preEvolucion = read $ datos!!10::Int,
                              evolucion = datos!!11}
   | otherwise = Especie {numero = read $ datos!!0::Int, 
                        nombreEspecie = datos!!1, 
                        tipoEspecie = tuplaTipo, 
                        hpEspecie = read $ datos!!4::Int,
                        ataque = read $ datos!!5::Int, 
                        defensa = read $ datos!!6::Int,
                        ataqueEspecial = read $ datos!!7::Int, 
                        defensaEspecial = read $ datos!!8::Int, 
                        velocidad = read $ datos!!9::Int,
                        preEvolucion = read $ datos!!10::Int,
                        evolucion = datos!!11}
      where
         tuplaTipo = read $ "("++datos!!2++","++datos!!3++")"::Either Tipo (Tipo,Tipo)

-- Obtiene la Especie que se encuentra en la posición indicada dentro del Pokedex.        
buscarEspecie :: [Especie] -> Int -> Especie
buscarEspecie pokedex numeroEspecie = pokedex!!numeroEspecie
  
-- Imprime por consola todos los datos del Pokedex de una Especie. 
imprimirEspecie :: Especie -> IO()
imprimirEspecie especie = do
   putStr "Número: "
   print $ numero especie
   putStr "Nombre: "
   print $ nombreEspecie especie
   putStr "Tipo: "
   print $ tipoEspecie especie
   putStr "HP: "
   print $ hpEspecie especie
   putStr "Ataque: "
   print $ ataque especie
   putStr "Defensa: "
   print $ defensa especie
   putStr "Ataque Especial: "
   print $ ataqueEspecial especie
   putStr "Defensa Especial: "
   print $ defensaEspecial especie
   putStr "Velocidad: "
   print $ velocidad especie

-- Crea un Ataque a partir de una lista de Strings con sus datos.
crearAtaque :: [String] -> Ataque
crearAtaque datos = Ataque {nombreAtaque = datos!!0,
                           tipoAtaque = read $ datos!!1::Tipo,
                           forma = read $ datos!!2::Bool,
                           puntoPoder = read $ datos!!3::Int,
                           poder = read $ datos!!4::Int}

-- Realiza la búsqueda binaria de un Ataque dentro del Ataquedex.
buscarAtaque :: [Ataque] -> String -> Int -> Int -> Maybe Ataque
buscarAtaque ataqueDex nombre cotaMinima cotaMaxima
   | cotaMaxima < cotaMinima = Nothing
   | nombreAtaque (ataqueDex!!mid) > nombre = buscarAtaque ataqueDex nombre cotaMinima (mid-1)
   | nombreAtaque (ataqueDex!!mid) < nombre = buscarAtaque ataqueDex nombre (mid+1) cotaMaxima
   | otherwise = Just (ataqueDex!!mid) 
      where
         mid = cotaMinima + ((cotaMaxima-cotaMinima) `div` 2)

-- Crea un Monstruo a partir de un lista de Strings con sus datos.
crearMonstruo :: [Especie] -> [Ataque] -> [String] -> Monstruo
crearMonstruo pokedex ataqueDex datos = 
   Monstruo {especie = especieMonstruo,
      sobreNombre = datos!!1,
      nivel = nivelMonstruo,
      hp = maxHp especieMonstruo nivelMonstruo,
      ataqueM = estadisticaAtaque especieMonstruo nivelMonstruo,
      defensaM = estadisticaDefensa especieMonstruo nivelMonstruo,
      ataqueEM = estadisticaAtaqueE especieMonstruo nivelMonstruo,
      defensaEM = estadisticaDefensaE especieMonstruo nivelMonstruo,
      velocidadM = estadisticaVelocidad especieMonstruo nivelMonstruo,
      ataque1 = buscarAtaque ataqueDex (datos!!3) 0 longitudAtaques,
      ataque2 = 
         if null $ datos!!4 then Nothing
         else buscarAtaque ataqueDex (datos!!4) 0 longitudAtaques,
      ataque3 =
         if null $ datos!!5 then Nothing
         else buscarAtaque ataqueDex (datos!!5) 0 longitudAtaques,
      ataque4 =
         if null $ datos!!6 then Nothing
         else buscarAtaque ataqueDex (datos!!6) 0 longitudAtaques}
   where
      especieMonstruo = buscarEspecie pokedex numeroEspecie
      nivelMonstruo = read $ datos!!2::Int
      numeroEspecie = read $ datos!!0::Int
      longitudAtaques = length ataqueDex
  
-- Imprime por consola la información de catálogo (del Pokedex) del Monstruo Actual
-- de un Entrenador.
imprimirMonstruo :: Monstruo -> IO()
imprimirMonstruo monstruo = do
   putStr "Especie: "
   print $ nombreEspecie $ especie monstruo
   putStr "Sobrenombre: "
   print $ sobreNombre monstruo
   putStr "Nivel: "
   print $ nivel monstruo
   putStr "HP: "
   print $ hp monstruo
   putStr "Ataque: "
   print $ ataqueM monstruo
   putStr "Defensa: "
   print $ defensaM monstruo
   putStr "Ataque Especial: "
   print $ ataqueEM monstruo
   putStr "Defensa Especial: "
   print $ defensaEM monstruo
   putStr "Velocidad: "
   print $ velocidadM monstruo

-- Calcula los Hit Points máximo de un Pokémon de acuerdo a su especie y su nivel.
maxHp ::  Especie -> Int -> Int
maxHp especie nivel = ((31 + 2 * hpEspecie especie + 255 `quot` 4 + 100) * nivel `div` 100 ) + 10  

-- Calcula el Ataque de un Pokémon de acuerdo a su especie y su nivel.
estadisticaAtaque :: Especie -> Int -> Int
estadisticaAtaque especie nivel = ((31 + 2 * ataque especie + 63) * nivel `div` 100 ) + 5 

-- Calcula la Defensa de un Pokémon de acuerdo a su especie y su nivel.
estadisticaDefensa :: Especie -> Int -> Int
estadisticaDefensa especie nivel = ((31 + 2 * defensa especie + 63) * nivel `div` 100 ) + 5 

-- Calcula el Ataque Especial de un Pokémon de acuerdo a su especie y su nivel.
estadisticaAtaqueE :: Especie -> Int -> Int
estadisticaAtaqueE especie nivel = ((31 + 2 * ataqueEspecial especie + 63) * nivel `div` 100 ) + 5 

-- Calcula la Defensa Especial de un Pokémon de acuerdo a su especie y su nivel.
estadisticaDefensaE :: Especie -> Int -> Int
estadisticaDefensaE especie nivel = ((31 + 2 * defensaEspecial especie + 63) * nivel `div` 100 ) + 5 

-- Calcula la Velocidad de un Pokémon de acuerdo a su especie y su nivel.
estadisticaVelocidad :: Especie -> Int -> Int
estadisticaVelocidad especie nivel = ((31 + 2 * velocidad especie + 63) * nivel `div` 100 ) + 5 

-- Determina, para un tipo de ataque, cuales tipos son super efectivos,
-- cuales tipos son resistentes y cuales son inmunes.
relacionAtaqueTipo :: Tipo      -- Tipo de ataque a determinar la relación.
                   -> ( [Tipo]  -- Tipos super efectivos a el (2x dano). 
                      , [Tipo]  -- Tipos resistentes a el (0.5x dano).
                      , [Tipo]  -- Tipos inmunes a el (0x dano).
                      )
relacionAtaqueTipo x
  | Bug      <- x = ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
  | Dark     <- x = ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  | Dragon   <- x = ([Dragon], [Steel], [])
  | Electric <- x = ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  | Fighting <- x = ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
  | Fire     <- x = ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  | Flying   <- x = ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  | Ghost    <- x = ([Ghost, Psychic], [Steel, Dark], [Normal])
  | Grass    <- x = ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
  | Ground   <- x = ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
  | Ice      <- x = ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  | Normal   <- x = ([], [Rock, Steel], [Ghost])
  | Poison   <- x = ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  | Psychic  <- x = ([Fighting, Poison], [Steel, Psychic], [Dark])
  | Rock     <- x = ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  | Steel    <- x = ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  | Water    <- x = ([Ground, Rock, Fire], [Water, Grass, Dragon], [])

-- Calcula el daño que produce un ataque de un Monstruo agresor sobre otro.
daño :: Monstruo -> Monstruo -> Ataque ->  Double
daño atacante defensor ataque = golpeFinal
  where
   golpeFinal = multiplicador (fromIntegral golpeTotal)
   multiplicador golpe = case tipoEspecie ( especie atacante ) of
      Left tipo          ->  if tipoAtaque ataque == tipo
                             then   (golpe) * 1.5
                             else golpe
      Right(tipo1,tipo2) ->  if  tipoAtaque ataque == tipo1 || tipoAtaque ataque == tipo2 
                             then   (golpe) * 1.5
                             else golpe
   golpeTotal =  modificadorAtacante golpe
   golpe = ((fuerzaAtacante * poderAtaque * lucha ) `div` 50 ) + 2
   fuerzaAtacante = (2 * nivel atacante `div` 5 ) + 2
   poderAtaque =  poder ataque
   lucha = case forma ataque of
      True -> ataqueM atacante `div` defensaM defensor
      False -> ataqueEM atacante `div`defensaEM defensor
   modificadorAtacante golpe =  case tipoEspecie ( especie atacante ) of 
      Left tipo ->  if primeraLista tipo
                    then golpe * 2
                    else 
                      if segundaLista tipo
                      then golpe `div` 2
                      else
                        if terceraLista tipo 
                        then 0 
                        else golpe
      Right (tipo1,tipo2) -> if primeraLista tipo1 && primeraLista tipo2
                             then golpe * 4
                             else
                              if (primeraLista tipo1 && not(primeraLista tipo2)) || 
                                 (not(primeraLista tipo1) && primeraLista tipo2)
                              then golpe * 2
                              else
                                 if segundaLista tipo1 && segundaLista tipo2
                                 then golpe `div` 4 
                                 else
                                    if (segundaLista tipo1 && not(segundaLista tipo2)) ||
                                       (not(segundaLista tipo1) && segundaLista tipo2)
                                    then golpe `div` 2
                                    else 
                                       if terceraLista tipo1 || terceraLista tipo2
                                       then 0
                                       else golpe 
  
   primeraLista tipo = elem tipo ((\(a,b,c)-> a)  (relacionAtaqueTipo  (tipoAtaque ataque)))
   segundaLista tipo = elem tipo ((\(a,b,c)-> b)  (relacionAtaqueTipo  (tipoAtaque ataque)))
   terceraLista tipo = elem tipo ((\(a,b,c)-> c)  (relacionAtaqueTipo  (tipoAtaque ataque)))
   
-- Crea el "Pokedex", lista que representa todas las especies de Pokemon que existen.
crearPokedex :: [[String]] -> [Especie]
crearPokedex listaEspecies = map crearEspecie listaEspecies
  
-- Crea el "Ataquedex", lista que representa todos los ataques que existen en el pokesim.
crearAtaquedex :: [[String]] -> [Ataque]
crearAtaquedex listaAtaques = map crearAtaque listaAtaques

-- Crea la lista de Monstruos que representa un Entrenador.
crearEntrenador :: [Especie] -> [Ataque] -> [[String]] -> [Monstruo]
crearEntrenador pokedex ataquedex listaMonstruos = map (crearMonstruo pokedex ataquedex) listaMonstruos

-- Imprime por consola la ayuda que ofrece pokesim, consistida en lista de ataques del
-- Monstruo actual y lista de Monstruos restantes
imprimirAyudaEntrenador :: [Monstruo] -> Monstruo -> IO()
imprimirAyudaEntrenador listaMonstruos actual = do
   putStrLn "Lista de Ataques:"
   putStr "Ataque 0: "
   print $ nombreAtaque $ fromJust(ataque1 actual)
   putStr "PP: "
   print $ puntoPoder $ fromJust(ataque1 actual)
   if ataque2 actual /= Nothing 
   then
      do
         putStr "Ataque 1: "
         print $ nombreAtaque $ fromJust(ataque2 actual)
         putStr "PP: "
         print $ puntoPoder $ fromJust(ataque2 actual)
   else
      putStrLn ""
   if ataque3 actual /= Nothing 
   then
      do
         putStr "Ataque 2: "
         print $ nombreAtaque $ fromJust(ataque3 actual)
         putStr "PP: "
         print $ puntoPoder $ fromJust(ataque3 actual)
   else
      putStrLn ""
   if ataque4 actual /= Nothing 
   then
      do
         putStr "Ataque 3: "
         print $ nombreAtaque $ fromJust(ataque4 actual)
         putStr "PP: "
         print $ puntoPoder $ fromJust(ataque4 actual)
   else
      putStrLn ""
   imprimirMonstruos (tail listaMonstruos) 1

-- Imprime la lista de Monstruos, sin contar el Monstruo Actual, de un Entrenador.
imprimirMonstruos :: [Monstruo] -> Int ->IO()
imprimirMonstruos (x:xs) i = do
   putStr $ nombreEspecie $ especie x
   putStrLn (show i)
   imprimirMonstruos xs (i+1)
   
-- Ejecuta el cambio de Monstruo solicitado por un Entrenador. Coloca el monstruo elegido
-- al principio de la lista de Monstruos respectiva.
cambiarMonstruo :: [Monstruo] -> Int -> [Monstruo]
cambiarMonstruo listaMonstruos numeroMonstruo 
   | 0<=numeroMonstruo && numeroMonstruo<longitudMonstruos = 
      head segundaMitad : (primeraMitad ++ tail segundaMitad)
   | otherwise = listaMonstruos!!(longitudMonstruos - 1):take (longitudMonstruos - 1) listaMonstruos
   where 
      longitudMonstruos = length listaMonstruos
      (primeraMitad, segundaMitad) = splitAt numeroMonstruo listaMonstruos
  
-- Evalua las velocidades de los Monstruos de cada Entrenador y indica cuál es más veloz.
evaluarVelocidad :: Monstruo -> Monstruo -> Int
evaluarVelocidad monstruo1 monstruo2
   | velocidadM monstruo1 >= velocidadM monstruo2 = 1
   | otherwise = 2
   
-- Actualiza las listas de Monstruos de los Entrenadores luego de aplicarAtaque.
actualizarEstado :: [Monstruo] -> [Monstruo] -> (Monstruo, Monstruo) -> ([Monstruo],[Monstruo])
actualizarEstado monstruos1 monstruos2 nuevoEstado = 
   (fst nuevoEstado:tail monstruos1, snd nuevoEstado:tail monstruos2)

-- Simula la ejecución de un ataque de un Monstruo a otro.
aplicarAtaque :: Monstruo -> Monstruo -> Ataque -> (Monstruo, Monstruo)
aplicarAtaque atacante defensor ataque = 
   (defensor{hp = hp defensor - dañoAtaque}, restarPoder atacante ataque)
   where
      dañoAtaque = round(daño atacante defensor ataque)

-- Resta un PP al Monstruo agresor, luego de utilizar su ataque.
restarPoder :: Monstruo -> Ataque -> Monstruo
restarPoder monstruo ataque =
   if ataque == fromJust(ataque1 monstruo)
   then monstruo {ataque1 = Just(ataque {puntoPoder = puntoPoder ataque - 1})}
   else
      if ataque == fromJust(ataque2 monstruo)
      then monstruo {ataque2 = Just(ataque {puntoPoder = puntoPoder ataque - 1})}
      else
         if ataque == fromJust(ataque3 monstruo)
         then monstruo {ataque3 = Just(ataque {puntoPoder = puntoPoder ataque - 1})}
         else
            monstruo {ataque4 = Just(ataque {puntoPoder = puntoPoder ataque - 1})}
      
-- Crea una estructura de tipo CampoBatalla, que contiene el pokedex, el ataquedex
-- y las dos listas de Monstruos de los Entrenadores.
crearCampoBatalla :: [Especie] -> [Ataque] -> [Monstruo] -> [Monstruo] -> CampoBatalla
crearCampoBatalla especies ataques trainer1 trainer2 =
   CampoBatalla {pokedex = especies,
               ataquedex = ataques,
               entrenador1 = trainer1,
               entrenador2 = trainer2}
               
-- Imprime por consola cada componente de la estructura CampoBatalla.
imprimirCampoBatalla :: CampoBatalla -> IO()
imprimirCampoBatalla campo = do
   print $ pokedex campo
   print $ ataquedex campo
   print $ entrenador1 campo
   print $ entrenador2 campo