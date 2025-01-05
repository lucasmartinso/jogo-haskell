-- LUCAS MARTINS OLIVEIRA - 202465058A
-- 

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)
> import Data.Array

> randomElement :: [Int] -> IO Int
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)

> main :: IO()  
> main = do 
>       putStrLn $ "BEM VINDO AO JOGO DOS PALITINHOS!!!!"
>       putStrLn $ "Escolha modo FACIL ou DIFICIL"
>       dificuldade <- getLine
>       putStrLn $ "Modo de dificuldade " ++ show dificuldade ++ " escolhido"
>       let num_fileiras = [2, 3 .. 10000]
>       let num_palitos = [1, 3 .. 7]
>       fileiras <- randomElement num_fileiras
>       palitos <- randomElement num_palitos
>       putStrLn $ "Numero de fileiras no jogo sera de " ++ show fileiras
>       putStrLn $ "Numero de palitos no jogo sera de " ++ show palitos
>       