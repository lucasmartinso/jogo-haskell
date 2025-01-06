-- LUCAS MARTINS OLIVEIRA - 202465058A
-- 

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)

> randomElement :: [Int] -> IO Int
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)

> removePalito :: Int -> Int -> [Int] -> IO[Int]
> removePalito fileira retirados lista
>           | (fileira-1) < 0 || (fileira-1) >= length lista = do
>                   putStrLn $ "ERRO: escolha uma fileira existente!!"
>                   return lista
>           | lista !! (fileira-1) == 0 = do
>                   putStrLn $ "ERRO: fileira escolhida ja esta vazia!!"
>                   return lista
>           | retirados < 1 = do 
>                   putStrLn $ "ERRO: tem que tirar ao menos um palito!!"
>                   return lista 
>           | retirados > lista !! (fileira-1) = do 
>                   putStrLn $ "ERRO: nao eh possivel retirar mais palitos do que existem na fileira!"
>                   return lista
>           | otherwise = return (take (fileira-1) lista ++ [lista !! (fileira-1) - retirados] ++ drop fileira lista) --remove a qntd de palitos desejada em dada fileira

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

-- > playing :: Bool -> Int -> [Int] -> Int
-- > playing True jogador jogo = return jogador
-- > playing False jogador jogo = do 
-- >                           if jogador == 1 then removePalito (length jogo) qntd_pal_aleat else --jogada do usuario
-- >                           acabou <- verifyEnd jogo
-- >                           playing acabou !jogador jogo

> verifyInvalidPlay :: [Int] -> [Int] -> IO(Bool)
> verifyInvalidPlay atual antigo = return (atual == antigo)


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