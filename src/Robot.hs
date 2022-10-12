module Robot ( readLDM
             , readLCR
             , run
             )where

import Control.Monad.State
import Parsing 



type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)

sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }

instance Show Robot where
  show (Robot x (a,b) y) = concat ["Energy:", show x, "\nPosition:", show (a,b), "\nCollected:", show y]

data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

instance Show Element where

   show (x) 
            |(x==Empty) = show " " -- espaco vazio 
            |(x==Entry) = show "E" -- entrada
            |(x==Wall) = show "%"  -- parede
            |(x==Earth) = show "." -- terra
            |(x==Rock) = show "*"  -- rocha
            |(x==Material) =       -- Material
            if x == 50 then "?"           
            else if x == 100 then ":"           
            else if x == 150 then ";"           
            else "$"

pElement :: Parser Char Element
pElement = f <$> (symbol ' ' <|> -- espaco vazio 
                  symbol 'E' <|> -- entrada
                  symbol '%' <|> -- parede
                  symbol '.' <|> -- terra
                  symbol '*' <|> -- rocha
                  symbol '?' <|> -- 50
                  symbol ':' <|> -- 100
                  symbol ';' <|> -- 150
                  symbol '$'     -- quantidade
                 )
            where
                f res
                  | res == ' ' = Empty
                  | res == 'E' = Entry
                  | res == '%' = Wall
                  | res == '.' = Earth
                  | res == '*' = Rock
                  | res == '?' = (Material 50)
                  | res == ':' = (Material 100)
                  | res == ';' = (Material 150)
                  | res == '$' = (Material 1)
                  | otherwise = error "Invalid Element"

type Line = [Element]

data Mine = Mine {
              lines    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
   -- ShowList (Mine a b xs) =(\y -> intercalate "" $ map show xs)
     show (Mine l c e) = unlines $ map (unwords . map show) e

findEntry :: Mine -> Point
findEntry m = (l, c)
  where
    ls = map (elemIndex Entry) (elements m)
    jc = head $ filter (/= Nothing) ls
    l = fromMaybe (-1) (elemIndex jc ls)
    c = fromMaybe (-1) jc

validMine :: Mine -> Bool
validMine (Mine 0 0 [])       = True 
validMine (Mine _ _ [])       = False 
validMine (Mine a b (x:xs)) = validaProporcoes (x:xs) a b && validaEntrada m where
   validaColuna ::[[Element]] -> Int ->Bool
   validaColuna (x:xs)_ =True
   validaColuna (x:xs)b = (length x ==b) && validaColuna

   validaProporcoes:: [[Element]] -> Int -> Int ->Bool
   validaProporcoes  xs a b = length xs == a && validaColuna xs b
   
   
  validaEntrada:: Mine  -> Point -> Bool
  validaEntrada (Mine l c (x:xs)) 
                |l' == 1||l' ==l  = if c'\= -1 then True else False
                |l' < l && l'> 1  = if  c' \= -1  && c' ==1||c' == length x  then True else False
                |otherwise = False
                where (l', c') = findEntry Mine
                
    --            | if findEntry m ==(l,-1) then False else  if c

  -- validaEntrada :: (Eq a, Num a) => [[Element]] -> a -> Int -> a-> Bool
   --validaEntrada [] _ _ _ = False
  --validaEntrada (x : xs) l c count 
   --                   | count == 0     = if elem Entry x then True else procuraEntrada xs l c (count+1)  
     --                 | count == (l-1) = if elem Entry x then True else False  
       --               | otherwise      = (fromMaybe (-1) $ elemIndex Entry x) == 0 || (fromMaybe (-1) $ elemIndex Entry x) == (c-1) || procuraEntrada xs l c (count+1) 

linha1 :: Line
linha1 = [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
linha2 :: Line
linha2 = [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Wall]
linha3 :: Line
linha3 = [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall]
linha4 :: Line
linha4 = [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Rock, Rock, Rock, Earth, Wall]
linha5 :: Line
linha5 = [Wall, Earth, (Material 50), Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall]
linha6 :: Line
linha6 = [Wall, Earth, Earth, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Wall]
linha7 :: Line
linha7 = [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall]
linha8 :: Line
linha8 = [Wall, Earth, (Material 100), Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall]
linha9 :: Line
linha9 = [Wall, Earth, Earth, Empty, Earth, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Wall]
linha10 :: Line
linha10 = [Wall, Earth, Earth, Rock, Earth, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Earth, Wall]
linha11 :: Line
linha11 = [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Empty, Earth, (Material 150), (Material 150), Earth, Earth, Wall]
linha12 :: Line
linha12 = [Wall, Earth, Rock, Earth, Earth, Empty, Earth, Earth, Earth, (Material 150), (Material 150), Earth, Earth, Rock, Wall]
linha13 :: Line
linha13 = [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, (Material 1), Wall]
linha14 :: Line
linha14 = [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Empty, Earth, Empty, Empty, Wall]
linha15 :: Line
linha15 = [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Entry, Wall]

exampleMine :: Mine
exampleMine =
  Mine
    { Robot.lines = 15,
      Robot.columns = 15,
      Robot.elements = [linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8, linha9, linha10, linha11, linha12, linha13, linha14, linha15]
    }

pLine :: Parser Char Line
pLine = f <$> pElement
      where
        f c = [head c]
--pLine = listOf pElement (symbol ’ ’)

pMine :: Parser Char Mine
pMine = transformaEmMina l <$> listOf pLine (symbol '\n') 
  where
  transformaEmMina::[Element]->Mine
  trasnformaEmMina l =  Mine
              { Robot.lines = length l,
                Robot.columns = length (head l),
                Robot.elements = l
              }


data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

pInst :: Parser Char Instr
pInst =     f <$> (symbol 'l' -- esquerda
                  <|>symbol 'L' -- esquerda
                  <|>symbol 'r' -- direita
                  <|>symbol 'R' -- direita
                  <|>symbol 'u' -- cima
                  <|>symbol 'U' -- cima
                  <|>symbol 'd' -- baixo
                  <|>symbol 'D' -- baixo
                  <|>symbol 's' -- recarregar
                  <|>symbol 'S'  -- recarregar
                 )
            where
                f res
                  | res == 'l' = L
                  | res == 'L' = L
                  | res == 'r' = R
                  | res == 'R' = R
                  | res == 'u' = U
                  | res == 'U' = U
                  | res == 'd' = D
                  | res == 'D' = D
                  | res == 's' = S
                  | res == "S" = S
                  | otherwise = error "Invalid Command"

pProgram :: Parser Char [Instr]
pProgram = greedy1 pInstr

type Conf = (Robot, Mine)

type ConfM a = State Conf a

current :: ConfM Point
current 
  =do
    (r,m) <- get
    return getPoint r
  let r' =position r : r
  put (r',m)
    --getPoint:: Robot -> Point
    --getPoint r = position r

mine :: ConfM Mine
mine = gets $ snd

getElement :: Mine -> Point -> Element
getElement m (x, y) = elements m !! x !! y


enoughEnergy :: Int -> ConfM Bool
enoughEnergy n
  =do
  (r,m) <- get
   let r' = if energy r > n then True else False

incEnergy :: ConfM ()
incEnergy 
  =do
    (r,m) <- get
    let  r' = getEnergyPlus r
    put (r',m)
    where 
      getEnergyPlus:: Robot->Fuel
      getEnergyPlus r = energy+1 r 

verificaParede :: Mine -> Point -> Bool
verificaParede m (x,y) = if elements m !! x !! y == Wall
                        then False
                        else True

verificaColeta :: Mine -> Point -> Bool
verificaMateriais m (x,y) = elements m !! (x+1) !! y == Material
                            || elements m !! (x-1) !! y == Material
                            || elements m !! x !! (y+1) == Material
                            || elements m !! x !! (y-1) == Material

valid :: Instr -> ConfM Bool
valid L
  = do
    (x, y) <- current
    mina <- getMine 
    element <- getElement mina (x-1,y)
    energy <- enoughEnergy energiaNecessaria element
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1
          return energy && verificaParede mina (x-1, y)

valid R
  = do
    (x, y) <- current
    mina <- getMine 
    element <- getElement mina (x+1,y)
    energy <- enoughEnergy energiaNecessaria element
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1
          return energy && verificaParede mina (x+1, y) 

valid U
  = do
    (x, y) <- current
    mina <- getMine 
    element <- getElement mina (x,y-1)
    energy <- enoughEnergy energiaNecessaria element
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1
          return energy && verificaParede mina (x, y-1) 

valid D
 (x, y) <- current
    mina <- getMine 
    element <- getElement mina (x,y+1)
    energy <- enoughEnergy energiaNecessaria element
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1
          return energy && verificaParede mina (x, y+1)

valid C
 = do
  energy <- enoughEnergy 10
  (x, y) <- current
  mina <- getMine
  return energy && verificaMateriais

valid S = return True 

achaMinerio :: Mine -> Point -> Point
achaMinerio m (x, y) = if elements m !! x+1 !! y == Material || Rock || Earth then (x+1,y)
                       else if elements m !! x-1 !! y == Material || Rock || Earth then (x-1,y)
                       else if elements m !! x !! y-1 == Material || Rock || Earth then (x,y-1)
                       else if elements m !! x !! y+1 == Material || Rock || Earth then (x,y+1)

updateMine :: Instr -> ConfM ()
-- updateMine L
--   = do
--     teste <- valid L
--     (x,y) <- current
--     (r, m) <- get
--     if teste == True
--       then modify (\(r, m) -> (r {position = (x-1,y), energy = energy r - 1}, m))
--       else return ()
-- updateMine R
--   = do
--     teste <- valid R
--     (x,y) <- current
--     (r, m) <- get
--     if teste == True
--       then modify (\(r, m) -> (r {position = (x+1,y), energy = energy r - 1}, m))
--       else return ()
-- updateMine U
--   = do
--     teste <- valid U
--     (x,y) <- current
--     (r, m) <- get
--     if teste == True
--       then modify (\(r, m) -> (r {position = (x,y-1), energy = energy r - 1}, m))
--       else return ()
-- updateMine D
--   = do
--     teste <- valid D
--     (x,y) <- current
--     (r, m) <- get
--     if teste == True
--       then modify (\(r, m) -> (r {position = (x,y+1), energy = energy r - 1}, m))
--       else return ()
updateMine C
  = do
    (x, y) <- current
    teste <- valid C
    (r,m) <- get
    (x, y) <- achaMinerio m (x,y)
    if teste == True then modify (\(r,m) -> (r, m{elements m !! x !! y = Empty}))
    else return ()
        



exec :: Instr -> ConfM ()
exec = undefined

initRobot :: Mine -> Robot
initRobot m =
  Robot
    { energy = 100,
      position = findEntry m,
      collected = 0
    }

readLDM :: String -> IO (Either String Mine)
readLDM = undefined

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined
