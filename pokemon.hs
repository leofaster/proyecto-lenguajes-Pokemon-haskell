module Pokemon
  (Tipo (Bug, Dark, Dragon, Electric, Fighting, Fire, Flying, Ghost, Grass, Ground, Ice, Normal, Poison, Psychic, Rock, Steel, Water)
  , maxHp
  , estadisticaAtaque
  , estadisticaDefensa
  , estadisticaAtaqueE
  , estadisticaDefensaE
  , estadisticaVelocidad
  , daño
  )
where
  

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
    , preEvolucion :: [Int]
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
  deriving (Read, Show)
  
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
    , ataque1 :: Ataque 
    , ataque2 :: Ataque
    , ataque3 :: Ataque
    , ataque4 :: Ataque
    }
  deriving (Read, Show)
  
maxHp ::  Monstruo -> Int
maxHp monstruo = ((31 + 2 * hpEspecie (especie monstruo) + 255 `quot` 4 + 100) * nivel monstruo `div` 100 ) + 10  

estadisticaAtaque :: Monstruo -> Int
estadisticaAtaque monstruo = ((31 + 2 * ataque (especie monstruo) + 63) * nivel monstruo `div` 100 ) + 5 

estadisticaDefensa :: Monstruo -> Int
estadisticaDefensa monstruo = ((31 + 2 * defensa (especie monstruo) + 63) * nivel monstruo `div` 100 ) + 5 

estadisticaAtaqueE :: Monstruo -> Int
estadisticaAtaqueE monstruo = ((31 + 2 * ataqueEspecial (especie monstruo) + 63) * nivel monstruo `div` 100 ) + 5 

estadisticaDefensaE :: Monstruo -> Int
estadisticaDefensaE monstruo = ((31 + 2 * defensaEspecial (especie monstruo) + 63) * nivel monstruo `div` 100 ) + 5 

estadisticaVelocidad :: Monstruo -> Int
estadisticaVelocidad monstruo = ((31 + 2 * velocidad (especie monstruo) + 63) * nivel monstruo `div` 100 ) + 5 

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


daño :: Monstruo -> Monstruo -> Ataque -> Int
daño atacante defensor ataque = golpeTotal 
  where
   golpeTotal =  modificadorAtacante golpe
   golpe = ((fuerzaAtacante * poderAtaque * lucha ) `div` 50 )+ 2
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
      Right (tipo1,tipo2) -> if primeraLista tipo1 || primeraLista tipo2
                             then golpe * 2
                             else
                              if segundaLista tipo1 || segundaLista tipo2
                              then golpe `div` 2
                              else 
                               if terceraLista tipo1 || terceraLista tipo2
                               then 0
                               else golpe 
      
      
   primeraLista tipo = elem tipo ((\(a,b,c)-> a)  (relacionAtaqueTipo  (tipoAtaque ataque)))
   segundaLista tipo = elem tipo ((\(a,b,c)-> b)  (relacionAtaqueTipo  (tipoAtaque ataque)))
   terceraLista tipo = elem tipo ((\(a,b,c)-> c)  (relacionAtaqueTipo  (tipoAtaque ataque)))
    