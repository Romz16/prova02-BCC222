module Robot ( readLDM
             , readLCR
             , run
             )where

import Control.Monad.State
import Parsing 
import Control.Monad
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Data.String
import System.IO
import Control.Monad
import Control.Exception
import Data.Traversable (for)

type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)
-- Seta valores para Robot
sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }
--1 Apresenta  a definição de Robot
instance Show Robot where
  show (Robot x (a,b) y) = concat ["Energy:", show x, "\nPosition:", show (a,b), "\nCollected:", show y]


data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

--2 Apresenta a Definição de Element
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

--3 Realiza Parser para o tipo Element
pElement :: Parser Char Element
--Possibilidades de simbolos que podem ser encontrados 
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
            --O que cada simbolo representará 
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

--7 Apresenta a Mina
instance Show Mine where
   -- ShowList (Mine a b xs) =(\y -> intercalate "" $ map show xs)
     show (Mine l c e) = unlines $ map (unwords . map show) e

--Função auxilar para encontrar entrada 
findEntry :: Mine -> Point
findEntry m = (l, c)
  where
    ls = map (elemIndex Entry) (elements m)
    jc = head $ filter (/= Nothing) ls
    l = fromMaybe (-1) (elemIndex jc ls)
    c = fromMaybe (-1) jc

-- 4 V1 primeira versao do metodo para validar a Mina 
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

--V2     segunda versão para validar a mina

validMine1 :: Mine -> Bool
validMine1 (Mine 0 0 [])       = True 
validMine1 (Mine _ _ [])       = False 
validMine1 (Mine a b (x:xs):t) = validaProporcoes (x:xs) a b && possuiEntradaNaBordaAuxiliar  0 0 a b ((x:xs):t)) where
   validaColuna ::[[Element]] -> Int ->Bool
   validaColuna (x:xs)_ =True
   validaColuna (x:xs)b = (length x ==b) && validaColuna

   validaProporcoes:: [[Element]] -> Int -> Int ->Bool
   validaProporcoes  xs a b = length xs == a && validaColuna xs b
   
  possuiEntradaNaBordaAuxiliar :: Int->Int->Int->Int->[[Element]]->Bool
  possuiEntradaNaBordaAuxiliar i j numeroDeLinhas numeroDeColunas [] = False
  possuiEntradaNaBordaAuxiliar i j numeroDeLinhas numeroDeColunas ((hh1:th1):t) = 
       if(i >= numeroDeLinhas)
        then False
       else if (j >= numeroDeColunas)
        then possuiEntradaNaBordaAuxiliar (i+1) 0 numeroDeLinhas numeroDeColunas ((hh1:th1):t)
       else if((ehEntradaEEstaNaBorda i j numeroDeLinhas numeroDeColunas (elementoDaPosicaoNaMatriz i j ((hh1:th1):t))) == True)
        then True
       else possuiEntradaNaBordaAuxiliar i (j+1) numeroDeLinhas numeroDeColunas ((hh1:th1):t)
   

-- %,%,%,%,%,%,%,%,%,%,%,%,%,%,%
-- %,*,*,*,.,.,.,.,.,.,.,.,.,.,%
-- %,*,*,*,.,.,., ,.,.,.,*,.,.,%
-- %,*,*,*,.,.,., ,.,.,*,*,*,.,%
-- %,.,?,.,.,.,., ,.,.,.,*,.,.,%
-- %,.,., , , , , ,.,., ,.,.,.,%
-- %,.,.,.,., ,.,.,.,., ,.,.,.,%
-- %,.,:,.,., ,.,.,.,., ,.,.,.,%
-- %,.,., ,., , , , , , , ,.,.,%
-- %,.,.,*,., ,.,., ,.,.,.,.,.,%
-- %,.,.,.,., ,.,., ,.,;,;,.,.,%
-- %,.,*,.,., ,.,.,.,;,;,.,.,*,%
-- %,.,.,.,.,.,.,.,.,.,.,.,.,$,%
-- %,.,.,.,.,.,.,.,.,., , , ,.,%
-- %,%,%,%,%,%,%,%,%,%,%,%,%,L,%
--5 representa uma mina 15X15
exampleMine :: Mine
exampleMine = (Mine 15 15 [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Rock,Rock,Rock,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Wall],[Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],[Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Rock,Rock,Rock,Earth,Wall],[Wall,Earth,(Material 50),Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],[Wall,Earth,Earth,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Wall],[Wall,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],[Wall,Earth,(Material 100),Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],[Wall,Earth,Earth,Empty,Earth,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Wall],[Wall,Earth,Earth,Rock,Earth,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Earth,Wall],[Wall,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Empty,Earth,(Material 150),(Material 150),Earth,Earth,Wall],[Wall,Earth,Rock,Earth,Earth,Empty,Earth,Earth,Earth,(Material 150),(Material 150),Earth,Earth,Rock,Wall],[Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,(Material 1),Wall],[Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Empty,Empty,Empty,Earth,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Entry,Wall]])

-- 6 Parser para desenvoler as linhas da matriz  e parser para gerar a mina
pLine :: Parser Char Line
pLine = greedy1 pElement

pMine :: Parser Char Mine
pMine = f<$> listOf pLine eol
          where
            eol = symbol '\n'
            f res = Mine (length res) (length (head res)) res

data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

--8 Parser para o tipo de instruções
pInst :: Parser Char Instr
pInst =     f <$> (symbol 'l' -- esquerda
                  <|>symbol 'L' -- esquerda
                  <|>symbol 'r' -- direita
                  <|>symbol 'R' -- direita
                  <|>symbol 'u' -- cima
                  <|>symbol 'U' -- cima
                  <|>symbol 'd' -- baixo
                  <|>symbol 'D' -- baixo
                  <|>symbol 'C'
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
                  | res == 'C' = C
                  | res == 's' = S
                  | res == "S" = S
                  | otherwise = error "Invalid Command"

 --9 Parser par lista de instruçoes                  
pProgram :: Parser Char [Instr]
pProgram = greedy1 pInstr

type Conf = (Robot, Mine)

type ConfM a = State Conf a

--10 
--10.1 devolve ponto
current :: ConfM Point
current 
  =do
    (r,m) <- get
    return getPoint r
  let r' =position r : r
  put (r',m)
    --getPoint:: Robot -> Point
    --getPoint r = position r

--10.2 retorna configuração atual da mina
mine :: ConfM Mine
mine = gets $ snd

getElement :: Mine -> Point -> Element
getElement m (x, y) = elements m !! x !! y

--10.3 retorna se robo tem energia suficiente 
enoughEnergy :: Int -> ConfM Bool
enoughEnergy n
  =do
  (r,m) <- get
   let r' = if energy r > n then True else False

--10.4 adiciona 1 à energia do robo
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

temMinerio :: Mine -> Point -> Bool
temMinerio m (x, y) 
          |  elements m !! x !! y == Material = True
          |  otherwise = False

 --11 Valida intrução                           
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
  return energy && verificaMateriais m (x,y)
  where
    verificaMateriais :: Mine -> Point -> Bool
    verificaMateriais m (x, y) = temMinerio m (x-1,y) || temMinerio m (x+1,y) || temMinerio m (x,y-1) || temMinerio m (x,y+1)


valid S = return True 

achaMinerio :: Mine -> Point -> Point
achaMinerio m (x, y) = if elements m !! x+1 !! y == Material || Rock || Earth then (x+1,y)
                       else if elements m !! x-1 !! y == Material || Rock || Earth then (x-1,y)
                       else if elements m !! x !! y-1 == Material || Rock || Earth then (x,y-1)
                       else if elements m !! x !! y+1 == Material || Rock || Earth then (x,y+1)

updatePosition :: Point -> ConfM ()
updatePosition p = modify (\(r, m) -> (r {position = p, energy = energy r - 1}, m))

updateElem :: Point -> CongM()
updateEleme (x,y) = modify(\(r,m)->(r,m{l,c elemnts !!x !!y = Empyt }))

--12 Atualiza Mina com base nas instruções 
updateMine :: Instr -> ConfM ()
updateMine L = do
  inst <- valid L
  (x, y) <- current
  if inst
    then updatePosition (x -1, y)
    else return ()
updateMine R = do
  inst <- valid L
  (x, y) <- current
  if inst
    then updatePosition (x +1, y)
    else return ()
updateMine U = do
  inst <- valid U
  (x, y) <- current
  if inst
    then updatePosition (x, y+1)
    else return ()
updateMine D = do
  inst <- valid D
  (x, y) <- current
  if inst
    then updatePosition (x , y-1)
    else return ()
updateMine C = do
      inst <- valid C
      (x, y) <- current
      if inst
        then updateElem (x , y)
        else return ()
updateMine S = do
      incEnergy
 
--13 Executa uma função caso seja valido
exec :: Instr -> ConfM ()
exec inst = updateMine inst 

initRobot :: Mine -> Robot
initRobot m =
        Robot
          { energy = 100,
            position = findEntry m,
            collected = 0
          }

confFromExec :: Conf->Instr->Conf
confFromExec ((Robot energy (pi,pj) collected),(Mine linhas colunas [])) instrucao = 
    ((Robot energy (pi,pj) collected),(Mine linhas colunas []))
        
confFromExec ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t))) instrucao = 
    snd (runState (exec instrucao) ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t))))
        
runAuxiliar :: [Instr] -> Conf -> Conf
runAuxiliar [] ((Robot energy (pi,pj) collected),(Mine linhas colunas [])) = 
    ((Robot energy (pi,pj) collected),(Mine linhas colunas []))
        
runAuxiliar [] ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t))) = 
    ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t)))
        
runAuxiliar (hInstrucoes:tInstrucoes) ((Robot energy (pi,pj) collected),(Mine linhas colunas [])) = 
    ((Robot energy (pi,pj) collected),(Mine linhas colunas []))
        
runAuxiliar (hInstrucoes:tInstrucoes) ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t))) = 
    (runAuxiliar tInstrucoes (confFromExec  ((Robot energy (pi,pj) collected),(Mine linhas colunas ((hh:th):t))) hInstrucoes))
        
        --tInstrucoes (executarInstrucao hInstrucoes (Mine linhas colunas ((hh:th):t)))
        
run :: [Instr] -> Mine -> Mine
run [] (Mine linhas colunas []) = (Mine linhas colunas [])
run [] (Mine linhas colunas ((hh:th):t)) = (Mine linhas colunas ((hh:th):t))
run (hInstrucoes:tInstrucoes) (Mine linhas colunas []) = (Mine linhas colunas [])
  run (hInstrucoes:tInstrucoes) (Mine linhas colunas ((hh:th):t)) =
    snd (runAuxiliar (hInstrucoes:tInstrucoes) ((initRobot (Mine linhas colunas ((hh:th):t))),(Mine linhas colunas ((hh:th):t))))

initRobot :: Mine -> Robot
initRobot m =
  Robot
    { energy = 100,
      position = findEntry m,
      collected = 0
    }

readLDM :: String -> IO (Either String Mine)
readLDM nomeDoArquivo = do
    s <- readFile (nomeDoArquivo) 
    let res = runParser (pMine)s
    let mina = fst $ head 
    if validMine mina
    then return (Right mina)
    else return (Left "Erro na Mina!!")
    

readLCR :: String -> IO (Either String [Instr])
readLCR nomeDoArquiv = do
    s <- readFile nomeDoArquivo
    let res = runParser(pProgram)s  
    let result = fst $ head res
    if prog /=[] && valid result
    then return (Right result)
    else return (Left "Erro ao ler arquivo")
