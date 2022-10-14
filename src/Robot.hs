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
import Data.List
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

--2 Apresenta a Definição de Element
data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

instance Show Element where

   show (x) 
            |(x == Empty) = show " " -- espaco vazio 
            |(x == Entry) = show "E" -- entrada
            |(x == Wall) = show "%"  -- parede
            |(x == Earth) = show "." -- terra
            |( x== Rock) = show "*"  -- rocha
            |(x == Material 50) = "?" -- Material       
            |(x == Material 100) = ":" -- Material         
            |(x == Material 150) = ";" -- Material     
            |(x == Material 1) = "$" -- Material

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

-- 4 Metodo para validar a Mina 
temZeroLinhas :: Int->Bool
temZeroLinhas linhas = 
    if(linhas == 0)
        then True
    else False

temZeroLinhasEZeroColunas :: Int->Int->Bool
temZeroLinhasEZeroColunas linhas colunas = 
    if((temZeroLinhas linhas)&&(colunas == 0))
        then True
    else False

temZeroLinhasOuZeroColunas :: Int->Int->Bool
temZeroLinhasOuZeroColunas linhas colunas = 
    if((temZeroLinhas linhas)||(colunas == 0))
        then True
    else False

matrizEstaDeAcordoComAsDimensoesAuxiliar :: Int->Int->[[a]]->Bool
matrizEstaDeAcordoComAsDimensoesAuxiliar linhas colunas [] = linhas == 0
matrizEstaDeAcordoComAsDimensoesAuxiliar linhas colunas ((hh1:th1):t) =
    if((length (hh1:th1)) /= colunas)
        then False
    else matrizEstaDeAcordoComAsDimensoesAuxiliar (linhas - 1) colunas t

matrizEstaDeAcordoComAsDimensoes :: Int->Int->[[a]]->Bool
matrizEstaDeAcordoComAsDimensoes linhas colunas [] = temZeroLinhasEZeroColunas linhas colunas
matrizEstaDeAcordoComAsDimensoes linhas colunas ((hh1:th1):t) =
    matrizEstaDeAcordoComAsDimensoesAuxiliar linhas colunas ((hh1:th1):t)

estamosNaBorda :: Int->Int->Int->Int->Bool
estamosNaBorda i j numeroDeLinhas numeroDeColunas =
    if((i == 0) || (j == 0) || (i == (numeroDeLinhas-1)) || (j == (numeroDeColunas-1)))
        then True
    else False

ehEntradaEEstaNaBorda :: Int->Int->Int->Int->Element->Bool
ehEntradaEEstaNaBorda i j numeroDeLinhas numeroDeColunas elemento = 
    if((estamosNaBorda i j numeroDeLinhas numeroDeColunas) == False)
        then False
    else if((show elemento) /= (show Entry))
        then False
    else True

elementoDaPosicaoNaMatriz :: Int->Int->[[a]]->a
elementoDaPosicaoNaMatriz i j ((hh1:th1):t) = 
    if(i == 0)
        then (hh1:th1) !! j
    else elementoDaPosicaoNaMatriz (i-1) j t

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

validMine :: Mine->Bool
validMine (Mine linhas colunas []) = False
validMine (Mine linhas colunas ((hh1:th1):t)) = 
    if(temZeroLinhasOuZeroColunas linhas colunas)
        then False
    else if((matrizEstaDeAcordoComAsDimensoes linhas colunas ((hh1:th1):t)) == False)
        then False
    else if((possuiEntradaNaBordaAuxiliar 0 0 linhas colunas ((hh1:th1):t)) == False)
        then False
    else True
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
                  | res == 'S' = S
                  | otherwise = error "Invalid Command"

 --9 Parser par lista de instruçoes 
pProgram :: Parser Char [Instr]
pProgram = greedy1 pInst

type Conf = (Robot, Mine)

type ConfM a = State Conf a

--10 
--10.1 devolve ponto
current :: ConfM Point
current = gets $ position . fst

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
  if energy r > n then return True else return False

--10.4 adiciona 1 à energia do robo
incEnergy :: ConfM ()
incEnergy 
  =do
    (r,m) <- get
    modify (\(r, m) -> (r {energy = energy r + 1}, m))

verificaParede :: Mine -> Point -> Bool
verificaParede m (x,y) = if elements m !! x !! y == Wall
                        then False
                        else True

temMinerio :: Mine -> Point -> Bool
temMinerio m (x, y) = elements m !! x !! y == Material 50 
                      || elements m !! x !! y == Material 100
                      || elements m !! x !! y == Material 150
                      || elements m !! x !! y == Material 1

 --11 Valida intrução 
valid :: Instr -> ConfM Bool
valid L
  = do
    (x, y) <- current
    mina <- mine 
    element <- getElement mina (x-1, y)
    energy <- enoughEnergy energiaNecessaria element
    return energy && verificaParede mina (x-1, y)
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1

valid R
  = do
    (x, y) <- current
    mina <- mine 
    element <- getElement mina (x+1,y)
    energy <- enoughEnergy energiaNecessaria element
    return energy && verificaParede mina (x+1, y)
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1

valid U
  = do
    (x, y) <- current
    mina <- mine 
    element <- getElement mina (x,y-1)
    energy <- enoughEnergy energiaNecessaria element
    return energy && verificaParede mina (x, y-1) 
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1

valid D
  = do
    (x, y) <- current
    mina <- mine 
    element <- getElement mina (x,y+1)
    energy <- enoughEnergy energiaNecessaria element
    return energy && verificaParede mina (x, y+1)
      where
        energiaNecessaria :: Element -> Int
        energiaNecessaria x = 
          if(x == Rock) then 30
          else if(x == Earth) then 5
          else 1

valid C
 = do
  energy <- enoughEnergy 10
  (x, y) <- current
  mina <- mine
  return energy && verificaMateriais mina (x,y)
  where
    verificaMateriais :: Mine -> Point -> Bool
    verificaMateriais m (x, y) = temMinerio m (x-1,y) || temMinerio m (x+1,y) || temMinerio m (x,y-1) || temMinerio m (x,y+1)


valid S = return True

--12 Atualiza Mina com base nas instruções 
updatePosition :: Point -> ConfM ()
updatePosition p = modify (\(r, m) -> (r {position = p, energy = energy r - 1}, m))

setEmpty :: Mine -> Point -> [Line]
setEmpty (Mine l c xs) (x, y) = (replace xs (x, (replace (xs !! x) (y, Empty))))

replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs


updateElem :: Point -> ConfM()
updateElem (x,y) = modify(\(r,m)->(r,m{elements = setEmpty m (x,y)}))


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
--14 Configuração inicial do robo
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
        
--15 Retorna a configuração final 
run :: [Instr] -> Mine -> Mine
run [] (Mine linhas colunas []) = (Mine linhas colunas [])
run [] (Mine linhas colunas ((hh:th):t)) = (Mine linhas colunas ((hh:th):t))
run (hInstrucoes:tInstrucoes) (Mine linhas colunas []) = (Mine linhas colunas [])
run (hInstrucoes:tInstrucoes) (Mine linhas colunas ((hh:th):t)) =
  snd (runAuxiliar (hInstrucoes:tInstrucoes) ((initRobot (Mine linhas colunas ((hh:th):t))),(Mine linhas colunas ((hh:th):t))))


--16 Le arquivo .ldm
readLDM :: String -> IO (Either String Mine)
readLDM nomeDoArquivo = do
    s <- readFile (nomeDoArquivo) 
    let res = runParser (pMine) s
    mina <- fst $ head 
    if validMine mina
    then return (Right mina)
    else return (Left "Erro na Mina!!")
    
--17 Le arquivo lcr
readLCR :: String -> IO (Either String [Instr])
readLCR nomeDoArquivo = do
    s <- readFile nomeDoArquivo
    let res = runParser(pProgram)s  
    let result = fst $ head res
    if result /= [] && valid result
    then return (Right result)
    else return (Left "Erro ao ler arquivo")
