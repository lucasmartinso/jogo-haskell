-- LUCAS MARTINS OLIVEIRA - 202465058A
-- 

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)

> randomElement :: [Int] -> IO Int
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)

> removePalito :: Int -> Int -> [Int] -> IO[Int]
> removePalito i novoValor lista = return (take i lista ++ [lista !! i - novoValor] ++ drop (i+1) lista) --remove a qntd de palitos desejada em dada fileira

> fillFileiras :: Int -> IO [Int]
> fillFileiras 0 = return []
> fillFileiras n = do
>               let num_palitos = [1, 3 .. 7]
>               palitos <- randomElement num_palitos
>               fileira <- fillFileiras(n-1)
>               return (palitos : fileira)

> printGame :: Int -> [Int] -> IO()
> printGame 0 jogo = putStrLn $ ""
> printGame n jogo = do
>               printGame (n-1) jogo
>               putStrLn $ "Fileira " ++ show n ++ " : " ++ show (jogo !! (n-1)) --printa as fileiras

> verifyEnd :: [Int] -> IO(Bool) 
> verifyEnd jogo = return $ foldr (\x acc -> x == 0 && acc) True jogo --verifica o final

> main :: IO()  
> main = do 
>       putStrLn $ "BEM VINDO AO JOGO DOS PALITINHOS!!!!"
>       putStrLn $ "Escolha do modo: digite 0 para FACIL ou digite 1 para DIFICIL"
>       dificuldadeString <- getLine
>       let dificuldade = read dificuldadeString :: Int
>       putStrLn $ "Modo de dificuldade " ++ (if dificuldade == 0 then "FACIL" else "DIFICIL") ++ " escolhido"
>       let num_fileiras = [2, 3 .. 10]
>       fileiras <- randomElement num_fileiras --vai gerar a qntd de fileiras
>       jogo <- fillFileiras fileiras  --cada elemento da lista vai ser uma fileira com a qntd de palitos nessa
>       printGame (length jogo) jogo
>       jogo <- removePalito 0 1 jogo
>       printGame (length jogo) jogo
>       putStrLn $ "FIM DE JOGO "