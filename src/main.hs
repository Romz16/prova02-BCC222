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

      