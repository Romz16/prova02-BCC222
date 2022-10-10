--EX1
instance Show Robot where
  show (Robot x (a,b) y) = concat ["Energy:", show x, "\nPosition:", show (a,b), "\nCollected:", show y]   
  
sampleRobot :: Robot


--EX2
instance Show Element where

   show (Element x) 
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

--EX3
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

--EX4

validMine :: Mine -> Bool

validMine (Mine 0 0 [])       = True 
validMine (Mine _ _ [])       = False 
validMine (Mine a b (x:xs)) = validaProporcoes (x:xs) l c && validaEntrada (x:xs) a b 0 where
   validaColuna ::[[Element]] -> Int ->Bool
   validaColuna (x:xs)_ =True
   validaColuna (x:xs)b = (lenght x ==b) && validaColuna

   validaProporcoes:: [[Element]] -> Int -> Int ->Bool
   validaProporcoes  xs a b = length xs == a && validaColune xs b
   
   
   validaEntrada :: (Eq a, Num a) => [[Element]] -> a -> Int -> a-> Bool
   validaEntrada [] _ _ _ = False
   validaEntrada (x : xs) l c count 
                      | count == 0     = if elem Entry x then True else procuraEntrada xs l c (count+1)  
                      | count == (l-1) = if elem Entry x then True else False  
                      | otherwise      = (fromMaybe (-1) $ elemIndex Entry x) == 0 || (fromMaybe (-1) $ elemIndex Entry x) == (c-1) || procuraEntrada xs l c (count+1) 

    
--EX5

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


--EX6
pLine :: Parser Char Line
pLine = Parser(\ inp -> case inp of
      []->[]
      (x:xs)-> if pElement x
                  then [(x,xs)]
                  else [])

pMine :: Parser Char Mine
pMine = Parser(\ inp -> case inp of
   []->[]
   (x:xs)-> if pLine x
      then[(x,xs)]
      else[])

--Ex7
instance Show Mine where
   ShowList (Mine a b (x:xs)) =(\y -> intercalate "" $ map show xs)

--EX8
pInst :: Parser Char Inst
pInst =     f <$> (symbol 'l' <|> -- esquerda
                  symbol 'L' <|> -- esquerda
                  symbol 'r' <|> -- direita
                  symbol 'R' <|> -- direita
                  symbol 'u' <|> -- cima
                  symbol 'U' <|> -- cima
                  symbol 'd' <|> -- baixo
                  symbol 'D' <|> -- baixo
                  symbol 's' <|> -- recarregar
                  symbol 'S' <|>  -- recarregar
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
