# Nim-Game Haskell

This is a simple nim game written in Haskell. It uses simple IO () and represents the nim-board that is randomly generated as a list of the type [5,4,3,2,1] where each number is the number of sticks in that row.

````
Fileira 1 : 5
Fileira 2 : 3
Fileira 3 : 1
Fileira 4 : 3
Fileira 5 : 7
````
## Rules

Two players take it turn about to remove one or more sticks from the end of a single row. The winner is the player who removes the last stick or sticks from the board.

## Modes 

- Easy: user always does the first play and computer makes randomly plays
- Difficult: computer always does the first play and computer makes intelligent play, using stochastic algorithm 

## Inicialization 
```
ghci 
:l palitos.lhs
main
```

# Example - easy mode
```
*Main> main

BEM VINDO AO JOGO DOS PALITINHOS!!!!
Escolha do modo: digite 0 para FACIL, ou digite 1 para DIFICIL

> 0

Modo de dificuldade FACIL

Fileira 1 : 5
Fileira 2 : 3
Fileira 3 : 1
Fileira 4 : 3
Fileira 5 : 7

SEU TURNO
Escolha uma fileira que deseja retirar palitos: 5
Escolha a quantidade de palitos que deseja remover da fileira 5: 7

TURNO DA MAQUINA

Fileira 1 : 5
Fileira 2 : 2
Fileira 3 : 1
Fileira 4 : 3
Fileira 5 : 0

SEU TURNO
> Escolha uma fileira que deseja retirar palitos: 1
> Escolha a quantidade de palitos que deseja remover da fileira 1: 5

TURNO DA MAQUINA

Fileira 1 : 0
Fileira 2 : 1
Fileira 3 : 1
Fileira 4 : 3
Fileira 5 : 0

SEU TURNO
> Escolha uma fileira que deseja retirar palitos: 4
> Escolha a quantidade de palitos que deseja remover da fileira 4: 3

TURNO DA MAQUINA

Fileira 1 : 0
Fileira 2 : 1
Fileira 3 : 0
Fileira 4 : 0
Fileira 5 : 0

SEU TURNO
> Escolha uma fileira que deseja retirar palitos: 2
> Escolha a quantidade de palitos que deseja remover da fileira 2: 1

FIM DE JOGO O VENCEDOR EH O(A) USUARIO!!
```

