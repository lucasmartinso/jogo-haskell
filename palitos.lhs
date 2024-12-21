-- LUCAS MARTINS OLIVEIRA - 202465058A
-- 

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)


> randomElement :: [Int] -> IO Int
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)


> main :: IO()  
> main = do 
>       let num_fileiras = [2, 3 .. 10]
>       fileiras <- randomElement num_fileiras
>       putStrLn $ "Numero de fileiras no jogo sera de " ++ show fileiras