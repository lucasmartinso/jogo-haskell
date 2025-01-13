-- LUCAS MARTINS OLIVEIRA - 202465058A
-- VICTORIA TIEMI FERREIRA YAMASHITA - 202365144AC

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)
> import Data.List (findIndex)

> randomElement :: [Int] -> IO(Int) --funcao que pega um valor aleatorio de um vetor --parametros: lista
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)

> removePalito :: Int -> Int -> [Int] -> IO[Int] --remove os palitos de uma fileira --parametros: indice da fileira, num de palitos que vai tirar, tabuleiro
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

> fillFileiras :: Int -> IO [Int] --preenche o tabuleiro --parametros: qntd de fileiras
> fillFileiras 0 = return []
> fillFileiras n = do
>               let num_palitos = [1, 3 .. 7]
>               palitos <- randomElement num_palitos
>               fileira <- fillFileiras(n-1)
>               return (palitos : fileira)

> printGame :: Int -> [Int] -> IO() --printa o jogo --parametros: qntd de fileiras, jogo
> printGame 0 jogo = putStrLn $ ""
> printGame n jogo = do
>               printGame (n-1) jogo
>               putStrLn $ "Fileira " ++ show n ++ " : " ++ show (jogo !! (n-1)) --printa as fileiras

> verifyEnd :: [Int] -> IO(Bool) --verifica se jogo acabou --parametros: jogo
> verifyEnd jogo = return $ foldr (\x acc -> x == 0 && acc) True jogo 

> verifyInvalidPlay :: [Int] -> [Int] -> IO(Bool) --verifica se a jogada eh invalida --parametros: jogo modificado, jogo antes de modificado
> verifyInvalidPlay atual antigo = return (atual == antigo)

> playingEasy :: Bool -> Int -> [Int] -> IO(Int) --aonde acontece o jogo --parametros: boleano(False=jogo continua, True=jogo acaba), jogador(0=usuario, 1=computer), jogo
> playingEasy True jogador jogo = return jogador 
> playingEasy False jogador jogo
>           | jogador == 0 = do
>                   printGame (length jogo) jogo
>                   putStrLn $ "SEU TURNO"
>                   putStrLn $ "Escolha uma fileira que deseja retirar palitos: "
>                   fileiraStr <- getLine
>                   isNaturalF <- validateAndPlay fileiraStr "Fileira"
>                   if isNaturalF == False then playingEasy False 0 jogo else do
>                   let fileira = read fileiraStr :: Int --transforma para inteiro
>                   putStrLn $ "Escolha a quantidade de palitos que deseja remover da fileira " ++ show fileira 
>                   palitosStr <- getLine
>                   isNaturalP <- validateAndPlay palitosStr "Palitos"
>                   if isNaturalP == False then playingEasy False 0 jogo else do
>                   let palitos = read palitosStr :: Int
>                   let estAntigo = jogo
>                   jogo <- removePalito fileira palitos jogo --faz a jogada
>                   final <- verifyEnd jogo --verifica o final
>                   if final == True then playingEasy True 0 jogo else do --se acaba chama a funcao dnv passando flag de encerramento(True), e o jogador da ultima jogada
>                       invalid <- verifyInvalidPlay estAntigo jogo --verifica se foi uma jogada valida, se sim continua e passa para o proximo jogador
>                       if invalid == True then playingEasy False 0 jogo else playingEasy False 1 jogo --se nao foi valida retorna ao jogador atual p refazer a jogada
>
>          | jogador == 1 = do 
>                   putStrLn $ "TURNO DA MAQUINA" 
>                   fileira_random <- randomElement [1,2 .. (length jogo)] --fileira aleatoria
>                   if jogo !! (fileira_random-1) == 0 then playingEasy False 1 jogo else do 
>                   palitos_random <- randomElement [1,2 .. (jogo !! (fileira_random-1))] --pega um valor aleatorio dada a qntd de palitos daquela fileira selecionada aleatoriamente
>                   let estAntigo = jogo
>                   jogo <- removePalito fileira_random palitos_random jogo
>                   final <- verifyEnd jogo 
>                   if final == True then playingEasy True 1 jogo else do
>                       invalid <- verifyInvalidPlay estAntigo jogo
>                       if invalid == True then playingEasy False 1 jogo else playingEasy False 0 jogo

> validateAndPlay :: String -> [Char] -> IO(Bool)
> validateAndPlay input tipo = 
>    case validPlayInput input of
>        Just palitos -> do
>            return True
>        Nothing -> do
>            putStrLn $ "" ++ tipo ++ " tem que inteiro maior do que zero"
>            return False

> catchInitGameError :: String -> Maybe Int
> catchInitGameError str =
>   case reads str of
>       [(n, "")] | n == 0 || n == 1 -> Just n --se eh 0 ou 1, retorna eles, se nao quebra, --conversao bem sucedida = [(n,"")], so numero sem strings
>       _ -> Nothing

> validPlayInput :: String -> Maybe Int
> validPlayInput str =
>   case reads str of
>       [(n, "")] | n >= 0 -> Just n --conversao bem sucedida = [(n,"")], so numero inteiro sem strings
>       _ -> Nothing 

> int2bin :: Int -> Int -> Int -> IO(Int)
> int2bin 0 acc _ = return acc 
> int2bin n acc peso = 
>    int2bin (n `div` 2) (acc + (n `mod` 2) * peso) (peso * 10) --peso representa a posicao decimal correta do bit

> binGame :: [Int] -> Int -> IO[Int]
> binGame jogo 0 = return []
> binGame jogo n = do 
>              let decimal = jogo !! (n-1)
>              bin <- int2bin decimal 0 1
>              binList <- binGame jogo (n-1)
>              return (bin : binList)

> playingHard :: Bool -> Int -> [Int] -> IO(Int) --aonde acontece o jogo --parametros: boleano(False=jogo continua, True=jogo acaba), jogador(0=usuario, 1=computer), jogo
> playingHard True jogador jogo = return jogador 
> playingHard False jogador jogo
>           | jogador == 0 = do
>                   printGame (length jogo) jogo
>                   putStrLn $ "SEU TURNO"
>                   putStrLn $ "Escolha uma fileira que deseja retirar palitos: "
>                   fileiraStr <- getLine
>                   isNaturalF <- validateAndPlay fileiraStr "Fileira"
>                   if isNaturalF == False then playingHard False 0 jogo else do
>                   let fileira = read fileiraStr :: Int --transforma para inteiro
>                   putStrLn $ "Escolha a quantidade de palitos que deseja remover da fileira " ++ show fileira 
>                   palitosStr <- getLine
>                   isNaturalP <- validateAndPlay palitosStr "Palitos"
>                   if isNaturalP == False then playingHard False 0 jogo else do
>                   let palitos = read palitosStr :: Int
>                   let estAntigo = jogo
>                   jogo <- removePalito fileira palitos jogo --faz a jogada
>                   final <- verifyEnd jogo --verifica o final
>                   if final == True then playingHard True 0 jogo else do --se acaba chama a funcao dnv passando flag de encerramento(True), e o jogador da ultima jogada
>                       invalid <- verifyInvalidPlay estAntigo jogo --verifica se foi uma jogada valida, se sim continua e passa para o proximo jogador
>                       if invalid == True then playingHard False 0 jogo else playingHard False 1 jogo --se nao foi valida retorna ao jogador atual p refazer a jogada
>
>          | jogador == 1 = do 
>                   putStrLn $ "TURNO DA MAQUINA" 
>                   binariosInvert <- binGame jogo (length jogo)
>                   let binarios = reverse binariosInvert
>                   let somaBin = sum binarios 
>                   casaDecImparInvert <- verifyImpares somaBin
>                   let casaDecImpar = reverse casaDecImparInvert
>                   val_vencedor <- bin2int casaDecImpar
>                   let fileira_vencedor = findElement val_vencedor jogo
>                   if fileira_vencedor == -1 then do
>                        fileira_random <- randomElement [1,2 .. (length jogo)] --fileira aleatoria
>                        if jogo !! (fileira_random-1) == 0 then playingHard False 1 jogo else do 
>                        palitos_random <- randomElement [1,2 .. (jogo !! (fileira_random-1))] --pega um valor aleatorio dada a qntd de palitos daquela fileira selecionada aleatoriamente
>                        let estAntigo = jogo
>                        jogo <- removePalito fileira_random palitos_random jogo
>                        final <- verifyEnd jogo 
>                        if final == True then playingHard True 1 jogo else do
>                          invalid <- verifyInvalidPlay estAntigo jogo
>                          if invalid == True then playingHard False 1 jogo else playingHard False 0 jogo
>                   else do 
>                       jogo <- removePalito (fileira_vencedor + 1) val_vencedor jogo
>                       final <- verifyEnd jogo 
>                       if final == True then playingHard True 1 jogo else do playingHard False 0 jogo


> findElement :: (Ord a) => a -> [a] -> Int
> findElement val jogo = case findIndex (>= val) jogo of
>   Just i  -> i
>   Nothing -> -1

> bin2int :: [Int] -> IO(Int)
> bin2int binary = return (foldl(\acc (i,x) -> acc + x*(2^i) ) 0 (zip[0..] (reverse binary)))

> verifyImpares :: Int -> IO[Int]
> verifyImpares 0 = return []
> verifyImpares soma = do 
>                   let impar = soma `mod` 2 
>                   impList <- verifyImpares (soma `div` 10)
>                   return (impar : impList)

> main :: IO()  
> main = do 
>       putStrLn $ "BEM VINDO AO JOGO DOS PALITINHOS!!!!"
>       putStrLn $ "Escolha do modo: digite 0 para FACIL, ou digite 1 para DIFICIL"
>       dificuldadeString <- getLine
>       case catchInitGameError dificuldadeString of
>           Just dificuldade -> putStrLn $ "Modo de dificuldade " ++ if dificuldade == 0 then "FACIL" else "DIFICIL"
>           Nothing -> do
>               putStrLn "Entrada inválida! Digite apenas 0 ou 1"
>               main
>       let dificuldade = read dificuldadeString :: Int
>       let num_fileiras = [2, 3 .. 10]
>       fileiras <- randomElement num_fileiras --vai gerar a qntd de fileiras
>       jogo <- fillFileiras fileiras  --cada elemento da lista vai ser uma fileira com a qntd de palitos nessa
>       vencedor <- if dificuldade == 0 then playingEasy False 0 jogo else playingHard False 1 jogo
>       let winner = if vencedor == 0 then "USUARIO" else "MAQUINA"
>       putStrLn $ "FIM DE JOGO O VENCEDOR EH O(A) " ++ winner