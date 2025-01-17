-- LUCAS MARTINS OLIVEIRA - 202465058A
-- VICTORIA TIEMI FERREIRA YAMASHITA - 202365144C

************************** JOGO DOS PALITINHOS **************************************

> import System.Random (randomRIO)
> import Data.List (elemIndex)


***FUNCAO DE ALEATORIZACAO****
--parametros: lista de inteiros
--retorna valor aleatorio presente na lista

> randomElement :: [Int] -> IO(Int)
> randomElement xs = do
>       index <- randomRIO (0, length xs - 1) -- Gera um índice aleatório
>       return (xs !! index)


***VALIDACOES DE TIPO***
--verifica se o input passado eh valido, ou seja, se eh um numero natural
--parametros: valor avaliado, o que representa esse valor(ex: 10 palitos, 10 fileiras, 10 etc...)
--retorna: True(eh natural), False(Nao eh natual)

> validateAndPlay :: [Char] -> [Char] -> IO(Bool)
> validateAndPlay input tipo = 
>    case validPlayInput input of
>        Just palitos -> do
>            return True
>        Nothing -> do
>            putStrLn $ "" ++ tipo ++ " tem que ser um inteiro maior do que zero"
>            return False

--verifica se o input passado para escolher modo de dificuldade eh valido(0 ou 1)
--parametros: valor do input fornecido pelo usuario
--retorna: valor se for valida, Nothing se input invalido

> catchInitGameError :: [Char] -> Maybe Int
> catchInitGameError str =
>   case reads str of
>       [(n, "")] | n == 0 || n == 1 -> Just n --conversao bem sucedida = [(n,"")], so numero sem strings
>       _ -> Nothing

--verifica se o input passado eh do tipo inteiro
--parametros: string que representa o input
--retorna: valor se valido, Nothing se for outro tipo alem de Int

> validPlayInput :: [Char] -> Maybe Int
> validPlayInput str =
>   case reads str of
>       [(n, "")] | n >= 0 -> Just n --conversao bem sucedida = [(n,"")], so numero inteiro sem strings
>       _ -> Nothing 


***INICIALIZACAO DO JOGO***
--preenche as fileiras
--parametros: numero de fileiras
--retorna: jogo(fileiras preenchidas com os palitos de forma aleatoria)

> fillFileiras :: Int -> IO [Int] --preenche o tabuleiro --parametros: qntd de fileiras
> fillFileiras 0 = return []
> fillFileiras n = do
>               let num_palitos = [1, 3 .. 7]
>               palitos <- randomElement num_palitos
>               fileira <- fillFileiras(n-1)
>               return (palitos : fileira)


***FUNCOES USADAS EM AMBOS OS MODOS DE DIFICULDADE****
--remove os palitos de uma determinada fileira
--parametros: indice da fileira, num de palitos que vai tirar, jogo
--retorna o jogo atualizado

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
>           | otherwise = return (take (fileira-1) lista ++ [lista !! (fileira-1) - retirados] ++ drop fileira lista)

--printa o estado atual do jogo
--parametros: tamanho do jogo, jogo

> printGame :: Int -> [Int] -> IO()
> printGame 0 jogo = putStrLn $ ""
> printGame n jogo = do
>               printGame (n-1) jogo
>               putStrLn $ "Fileira " ++ show n ++ " : " ++ show (jogo !! (n-1))

--verifica o final do jogo, se todos os elementos da lista que representa o jogo sao iguais a 0
--parametros: jogo
--retorna: booleano, True("acabou") False("continua")

> verifyEnd :: [Int] -> IO(Bool)
> verifyEnd jogo = return $ foldr (\x acc -> x == 0 && acc) True jogo 

--verifica se uma jogada eh invalida, ou seja, tentou fez uma jogada que nao mudou o estado antigo do jogo
--parametros: jogo antes da jogada, jogo atual
--retorna: booleano, True("invalida") False("valida")

> verifyInvalidPlay :: [Int] -> [Int] -> IO(Bool) --verifica se a jogada eh invalida --parametros: jogo modificado, jogo antes de modificado
> verifyInvalidPlay atual antigo = return (atual == antigo)


***FUNCOES USADAS NO MODO DIFICIL***
--converte inteiro para binario
--parametros: valor a ser convertido, acumulador(valor que vai representar o binario), parte do numero que esta sendo convertida
--retorna: valor em binario como se fosse um inteiro

> int2bin :: Int -> Int -> Int -> IO(Int)
> int2bin 0 acc _ = return acc 
> int2bin n acc peso = 
>    int2bin (n `div` 2) (acc + (n `mod` 2) * peso) (peso * 10) --peso representa a posicao decimal correta do bit

--converte binario para inteiro
--parametros: binario que esta em forma de lista
--retorna: valor inteiro que representa o binario

> bin2int :: [Int] -> IO(Int)
> bin2int binary = return (foldl(\acc (i,x) -> acc + x*(2^i) ) 0 (zip[0..] (reverse binary)))

--converte jogo todo para binario
--parametros: jogo, tamanho do jogo
--retorna: valor em binario como se fosse um inteiro

> binGame :: [Int] -> Int -> IO[Int]
> binGame jogo 0 = return []
> binGame jogo n = do 
>              let decimal = jogo !! (n-1)
>              bin <- int2bin decimal 0 1
>              binList <- binGame jogo (n-1)
>              return (bin : binList)

--verifica no binario qual "casa decimal" eh impar, qual eh par 
--parametros: binario 
--retorna: lista em que cada posicao representa se a "casa decimal" eh par ou impar, se par eh 0, se impar 1

> verifyImpares :: Int -> IO[Int]
> verifyImpares 0 = return []
> verifyImpares soma = do 
>                   let impar = soma `mod` 2 
>                   impList <- verifyImpares (soma `div` 10)
>                   return (impar : impList)

--converte inteiro em uma lista, em que cada casa decimal eh uma posicao da lista
--parametros: valor a ser convertido
--retorna: valor inteiro em representacao de lista

> intToList :: Int -> IO[Int]
> intToList 0 = return []
> intToList n = do
>    rest <- intToList (n `div` 10) -- Obtém a parte recursiva dentro do IO
>    return (rest ++ [n `mod` 10])  -- Concatena e retorna dentro do IO

--verifica se determinada fileira eh valida para fazer a jogada vencedora
--parametros: valor vencedor a ser removido, jogo, tamanho do jogo
--retorna: indice da fileira que satisfaz a condicao, se a fileira nao existir -1

> matchPatterns :: [Int] -> [Int] -> Int -> IO(Int) 
> matchPatterns palit_vence jogo 0 = return (-1)
> matchPatterns palit_vence jogo n = do
>                                let elem_jogo = jogo !! (n-1)
>                                elem_jogo_convertido <- intToList elem_jogo 
>                                let iguala_bin = length elem_jogo_convertido - length palit_vence
>                                candidato <- comparising palit_vence (drop iguala_bin $ elem_jogo_convertido)
>                                if candidato == True then return (n-1) else matchPatterns palit_vence jogo (n-1)

--verificacao se a fileira em binario corresponde ao binario a ser retirado, aonde bin_retirado for 1 binario da fileira tem que ser 1, se bin_retirado for 0 continua independente do binario da fileira
--parametros: valor vencedor a ser removido, jogo, tamanho do jogo
--retorna: indice da fileira que satisfaz a condicao, se a fileira nao existir -1

> comparising :: [Int] -> [Int] -> IO(Bool)
> comparising [] [] = return True
> comparising _ [] = return False
> comparising [] _ = return False
> comparising (x:xs) (y:ys)
>    | x == 0    = comparising xs ys -- Se `x` for 0, continua normalmente
>    | y == 1    = comparising xs ys -- Se `x` for 1 e `y` for 1, continua
>    | otherwise = return False

> minExceptZero :: (Ord a, Num a) => [a] -> a
> minExceptZero lista =
>    let filtered = filter (/= 0) lista
>    in if null filtered then 0 else minimum filtered

> normalizeBinRetire :: [Int] -> IO(Int, Int)
> normalizeBinRetire jogo = do 
>                       let maior = maximum jogo 
>                       let menor = minExceptZero jogo
>                       let x = maior - menor
>                       fileira <- indexBigger jogo
>                       return (fileira, x)    

> indexBigger :: (Ord a) => [a] -> IO(Int)
> indexBigger lista = return $ case elemIndex (maximum lista) lista of
>    Just i  -> i
>    Nothing -> -1


***MODO FACIL***
--representa modo de jogo facil, usa as funcoes previamente implementadas
--parametros: Flag de fim de jogo, jogador que faz a proxima jogada
--retorna: jogador vencedor

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
>                       let fileira = read fileiraStr :: Int --transforma para inteiro
>                       putStrLn $ "Escolha a quantidade de palitos que deseja remover da fileira " ++ show fileira 
>                       palitosStr <- getLine
>                       isNaturalP <- validateAndPlay palitosStr "Palitos"
>                       if isNaturalP == False then playingEasy False 0 jogo else do
>                           let palitos = read palitosStr :: Int
>                           let estAntigo = jogo
>                           jogo <- removePalito fileira palitos jogo --faz a jogada
>                           final <- verifyEnd jogo --verifica o final
>                           if final == True then playingEasy True 0 jogo else do --se acaba chama a funcao dnv passando flag de encerramento(True), e o jogador da ultima jogada
>                               invalid <- verifyInvalidPlay estAntigo jogo --verifica se foi uma jogada valida, se sim continua e passa para o proximo jogador
>                               if invalid == True then playingEasy False 0 jogo else playingEasy False 1 jogo --se nao foi valida retorna ao jogador atual p refazer a jogada
>
>          | jogador == 1 = do 
>                   putStrLn $ "TURNO DA MAQUINA" 
>                   fileira_random <- randomElement [1,2 .. (length jogo)] --fileira aleatoria
>                   if jogo !! (fileira_random-1) == 0 then playingEasy False 1 jogo else do 
>                       palitos_random <- randomElement [1,2 .. (jogo !! (fileira_random-1))] --pega um valor aleatorio dada a qntd de palitos daquela fileira selecionada aleatoriamente
>                       let estAntigo = jogo
>                       jogo <- removePalito fileira_random palitos_random jogo
>                       final <- verifyEnd jogo 
>                       if final == True then playingEasy True 1 jogo else do
>                           invalid <- verifyInvalidPlay estAntigo jogo
>                           if invalid == True then playingEasy False 1 jogo else playingEasy False 0 jogo

***MODO DIFICIL***
--representa modo de jogo dificil, usa as funcoes previamente implementadas
--parametros: Flag de fim de jogo, jogador que faz a proxima jogada
--retorna: jogador vencedor

> playingHard :: Bool -> Int -> [Int] -> Int -> IO(Int) --aonde acontece o jogo --parametros: boleano(False=jogo continua, True=jogo acaba), jogador(0=usuario, 1=computer), jogo
> playingHard True jogador jogo try = return jogador 
> playingHard False jogador jogo try
>           | jogador == 0 = do
>                   printGame (length jogo) jogo
>                   putStrLn $ "SEU TURNO"
>                   putStrLn $ "Escolha uma fileira que deseja retirar palitos: "
>                   fileiraStr <- getLine
>                   isNaturalF <- validateAndPlay fileiraStr "Fileira"
>                   if isNaturalF == False then playingHard False 0 jogo 0 else do
>                       let fileira = read fileiraStr :: Int --transforma para inteiro
>                       putStrLn $ "Escolha a quantidade de palitos que deseja remover da fileira " ++ show fileira 
>                       palitosStr <- getLine
>                       isNaturalP <- validateAndPlay palitosStr "Palitos"
>                       if isNaturalP == False then playingHard False 0 jogo 0 else do
>                       let palitos = read palitosStr :: Int
>                       let estAntigo = jogo
>                       jogo <- removePalito fileira palitos jogo --faz a jogada
>                       final <- verifyEnd jogo --verifica o final
>                       if final == True then playingHard True 0 jogo 0 else do --se acaba chama a funcao dnv passando flag de encerramento(True), e o jogador da ultima jogada
>                           invalid <- verifyInvalidPlay estAntigo jogo --verifica se foi uma jogada valida, se sim continua e passa para o proximo jogador
>                           if invalid == True then playingHard False 0 jogo 0 else playingHard False 1 jogo 0 --se nao foi valida retorna ao jogador atual p refazer a jogada
>
>          | jogador == 1 = do 
>                   if try == 0 then putStrLn $ "TURNO DA MAQUINA" else return ()
>                   binariosInvert <- binGame jogo (length jogo)
>                   let binarios = reverse binariosInvert
>                   let somaBin = sum binarios 
>                   casaDecImparInvert <- verifyImpares somaBin
>                   let casaDecImpar = reverse casaDecImparInvert
>                   val_vencedor <- bin2int casaDecImpar
>                   fileira_vencedor <- matchPatterns casaDecImpar binarios (length jogo)
>                   if fileira_vencedor == (-1) || val_vencedor == 0 then do
>                        if val_vencedor == 0 then do
>                           fileira_random <- randomElement [1,2 .. (length jogo)] --fileira aleatoria
>                           if jogo !! (fileira_random-1) == 0 then playingHard False 1 jogo 0 else do 
>                               palitos_random <- randomElement [1,2 .. (jogo !! (fileira_random-1))] --pega um valor aleatorio dada a qntd de palitos daquela fileira selecionada aleatoriamente
>                               let estAntigo = jogo
>                               jogo <- removePalito fileira_random palitos_random jogo
>                               final <- verifyEnd jogo 
>                               if final == True then playingHard True 1 jogo 0 else do
>                                   invalid <- verifyInvalidPlay estAntigo jogo
>                                   if invalid == True then playingHard False 1 jogo 0 else playingHard False 0 jogo 0
>                        else do
>                           let ant_state = jogo
>                           ajuste_val_venc <- normalizeBinRetire jogo
>                           jogo <- removePalito ((fst ajuste_val_venc) + 1) (snd ajuste_val_venc) jogo
>                           binary <- binGame jogo (length jogo)
>                           impares <- verifyImpares (sum binary)
>                           let isPar = sum impares
>                           if isPar == 0 then do
>                               final <- verifyEnd jogo 
>                               if final == True then playingHard True 1 jogo 0 else do playingHard False 0 jogo 0
>                           else do 
>                               fileira_random <- randomElement [1,2 .. (length ant_state)] --fileira aleatoria
>                               if ant_state !! (fileira_random-1) == 0 then playingHard False 1 ant_state (try + 1) else do 
>                                   palitos_random <- randomElement [1,2 .. (ant_state !! (fileira_random-1))] --pega um valor aleatorio dada a qntd de palitos daquela fileira selecionada aleatoriamente
>                                   let estAntigo = ant_state
>                                   jogo <- removePalito fileira_random palitos_random ant_state
>                                   final <- verifyEnd jogo 
>                                   binary <- binGame jogo (length jogo)
>                                   impares <- verifyImpares (sum binary) 
>                                   let newImpares = if try >= 80 then [0,0,0] else impares
>                                   if (sum newImpares) /= 0 then playingHard False 1 estAntigo (try + 1) else do  
>                                       if final == True then playingHard True 1 jogo 0 else do
>                                           invalid <- verifyInvalidPlay estAntigo jogo
>                                           if invalid == True then playingHard False 1 jogo (try + 1) else playingHard False 0 jogo 0
>                   else do 
>                       jogo <- removePalito (fileira_vencedor + 1) val_vencedor jogo
>                       final <- verifyEnd jogo 
>                       if final == True then playingHard True 1 jogo 0 else do playingHard False 0 jogo 0


**MAIN**
--aonde inicia o jogo
--parametros: nenhum 
--retorna: mensagem com o nome do vencedor(usuario ou maquina)

> main :: IO()  
> main = do 
>       putStrLn $ ""
>       putStrLn $ "BEM VINDO AO JOGO DOS PALITINHOS!!!!"
>       putStrLn $ "Escolha do modo: digite 0 para FACIL, ou digite 1 para DIFICIL"
>       dificuldadeString <- getLine
>       case catchInitGameError dificuldadeString of
>           Just dificuldade -> putStrLn $ "Modo de dificuldade " ++ if dificuldade == 0 then "FACIL" else "DIFICIL"
>           Nothing -> do
>               putStrLn "Entrada inválida! Digite apenas 0 ou 1"
>               main
>       let dificuldade = read dificuldadeString :: Int
>       let num_fileiras = [2, 3 .. 100]
>       fileiras <- randomElement num_fileiras --vai gerar a qntd de fileiras
>       jogo <- fillFileiras fileiras  --cada elemento da lista vai ser uma fileira com a qntd de palitos nessa
>       vencedor <- if dificuldade == 0 then playingEasy False 0 jogo else playingHard False 1 jogo 0
>       let winner = if vencedor == 0 then "USUARIO" else "MAQUINA"
>       putStrLn $ "FIM DE JOGO O VENCEDOR EH O(A) " ++ winner
