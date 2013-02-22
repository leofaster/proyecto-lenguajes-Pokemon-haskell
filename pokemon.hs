module Pokemon
  (Tipo (Bug, Dark, Dragon, Electric, Fighting, Fire, Flying, Ghost, Grass, Ground, Ice, Normal, Poison, Psychic, Rock, Steel, Water)
  , maxHp
  , estadisticaAtaque
  , estadisticaDefensa
  , estadisticaAtaqueE
  , estadisticaDefensaE
  , estadisticaVelocidad
  , daño
  , crearPokedex
  , crearAtaquedex
  , buscarEspecie
  , imprimirEspecie
  , buscarAtaque
  )
where

import Data.List
import Data.Char
import Data.Array

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
         
buscarEspecie :: Array Int Especie -> Int -> Especie
buscarEspecie pokedex numeroEspecie = pokedex!numeroEspecie
  
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
   
  
data Ataque
  = Ataque
    { nombreAtaque :: String
    , tipoAtaque :: Tipo 
    , forma :: Bool
    , puntoPoder :: Int 
    , poder :: Int 
    }
  deriving (Read, Show)
  
crearAtaque :: [String] -> Ataque
crearAtaque datos = Ataque {nombreAtaque = datos!!0,
                           tipoAtaque = read $ datos!!1::Tipo,
                           forma = read $ datos!!2::Bool,
                           puntoPoder = read $ datos!!3::Int,
                           poder = read $ datos!!4::Int}


buscarAtaque :: [Ataque] -> String -> Int -> Int -> Maybe Ataque
buscarAtaque ataqueDex nombre cotaMinima cotaMaxima
   | cotaMaxima < cotaMinima = Nothing
   | nombreAtaque (ataqueDex!!mid) > nombre = buscarAtaque ataqueDex nombre cotaMinima (mid-1)
   | nombreAtaque (ataqueDex!!mid) < nombre = buscarAtaque ataqueDex nombre (mid+1) cotaMaxima
   | otherwise = Just (ataqueDex!!mid) 
      where
         mid = cotaMinima + ((cotaMaxima-cotaMinima) `div` 2)

   
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
  
crearMonstruo :: Array Int Especie -> [Ataque] -> [String] -> Monstruo
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
  
maxHp ::  Especie -> Int -> Int
maxHp especie nivel = ((31 + 2 * hpEspecie especie + 255 `quot` 4 + 100) * nivel `div` 100 ) + 10  

estadisticaAtaque :: Especie -> Int -> Int
estadisticaAtaque especie nivel = ((31 + 2 * ataque especie + 63) * nivel `div` 100 ) + 5 

estadisticaDefensa :: Especie -> Int -> Int
estadisticaDefensa especie nivel = ((31 + 2 * defensa especie + 63) * nivel `div` 100 ) + 5 

estadisticaAtaqueE :: Especie -> Int -> Int
estadisticaAtaqueE especie nivel = ((31 + 2 * ataqueEspecial especie + 63) * nivel `div` 100 ) + 5 

estadisticaDefensaE :: Especie -> Int -> Int
estadisticaDefensaE especie nivel = ((31 + 2 * defensaEspecial especie + 63) * nivel `div` 100 ) + 5 

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
   
crearPokedex :: [[String]] -> Array Int Especie
crearPokedex listaEspecies = array (1,251) [(i, x) | i <- [1..251], x <- [(!!) especies (i-1)]]
   where
      especies = map crearEspecie listaEspecies
      
crearAtaquedex :: [[String]] -> [Ataque]
crearAtaquedex listaAtaques = map crearAtaque listaAtaques
