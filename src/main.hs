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
validMine (Mine a b (x:xs)) = validaProporcoes (x:xs) l c && validaEntrada (x:xs) where
   validaProporcoes  xs a b = length xs == a && length xs == b
   validaEntrada [] _ _  = False
   validaEntrada x = sat(x Entry)
    

                            

module Main (main) where

    import Robot
    import System.Environment (getArgs)
    
    {- 
    stack build
    stack exec prova02-exe
    -}
    
    main :: IO ()
    --main = run
    main = do
              args <- getArgs
              doIt args
    
    doIt :: [String] -> IO ()
    doIt [fm , fr]
       = do
            pm <- readLDM fm
            pr <- readLCR fr
            let
               m = either error id pm
               r = either error id pr
               m' = run r m
            print m'
    doIt _ = putStrLn "Informe arquivos de entrada!"

