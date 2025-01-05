-- LUCAS MARTINS OLIVEIRA - 202465058A
-- 

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)

> randomElement :: [Int] -> IO Int
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)

> data Stack a = Stack [a] deriving (Show)

> push :: Stack a -> a -> Stack a
> push (Stack xs) x = Stack (x:xs)
> pop :: Stack a -> (a, Stack a)
> pop (Stack (x:xs)) = (x, Stack xs)

> main :: IO()  
> main = do 
>       let num_fileiras = [2, 3 .. 10000]
>       let num_palitos = [1, 3 .. 7]
>       fileiras <- randomElement num_fileiras
>       palitos <- randomElement num_palitos
>       putStrLn $ "Numero de fileiras no jogo sera de " ++ show fileiras
>       putStrLn $ "Numero de palitos no jogo sera de " ++ show palitos