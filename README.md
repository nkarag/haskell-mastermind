# mastermind
This is a command-line version of the well-known [mastermind game](https://en.wikipedia.org/wiki/Mastermind_%28board_game%29).
## How to play
The game is played with 4 digit numbers DDDD where D in [0,9] and the player  plays against the computer.
The player goes first and chooses a 4 digit secret code (in her mind). Then, the computer starts the guessing process. For each guess code proposed by the computer, the player must answer how many "Blacks" and how many "Whites" have been found.
* A Black corresponds to a correct digit that is placed also in the correct position
* A White corresponds to a correct digit that is placed in a wrong position.
It is crucial that the answer given by the player of how many black and whites correspond to the computer's guess code, is correct, otherwise the computer will run out of candidate codes and will terminate.

When the computer proposes a guess code that corresponds to 4 Blacks, then it has managed to find the secret code. The computer then notes the number of moves it took in order to find the secret code.

Next, it is the turn of the player to do the guessing. So the computer chooses a new secret code and probes the player to provide a guess code. Then the computer replies with the number of Blacks and Whites for the specific guess code. This repeats until the player manages to get 4 Blacks, which is the point that she has managed to successfully guess the secret code.

The player (computer or human) who has managed to guess the secret code with the smaller number of moves (guesses) is the winner of the game!
Following is a snapshot of a game session:

        $ stack exec -- mastermind
    Welcome!
    Lets play a game of Mastermind!
    
    You go first.
    Choose your secret code (a 4 digit number)
    Press <enter> when you are ready
    
    Is it 1122?
    How many Blacks? (0 - 4)
    1
    thank you!
    
    How many Whites?
    1
    Is it 0012?
    How many Blacks? (0 - 4)
    0
    thank you!
    
    How many Whites?
    1
    Is it 1331?
    How many Blacks? (0 - 4)
    0
    thank you!
    
    How many Whites?
    0
    Is it 2224?
    How many Blacks? (0 - 4)
    2
    thank you!
    
    How many Whites?
    1
    Is it 2425?
    How many Blacks? (0 - 4)
    4
    
    !!!!!!!!!!!!!!!!!!
    
    Great! I have found the secret code in 5 moves!
    Am I an awesome player or what?
    
    OK. Now its your turn!
    I will choose a secret code.
    Press <enter> when you are ready
    
    
    OK. Lets start!
    
    What is your best guess?
    Give your guess (a 4-digit number): 1111
    Number of Blacks = 0
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 2222
    Number of Blacks = 2
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 2233
    Number of Blacks = 1
    Number of Whites = 1
    What is your best guess?
    Give your guess (a 4-digit number): 4224
    Number of Blacks = 1
    Number of Whites = 1
    What is your best guess?
    Give your guess (a 4-digit number): 5525
    Number of Blacks = 1
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 5522
    Number of Blacks = 1
    Number of Whites = 1
    What is your best guess?
    Give your guess (a 4-digit number): 6262
    Number of Blacks = 0
    Number of Whites = 2
    What is your best guess?
    Give your guess (a 4-digit number): 2727
    Number of Blacks = 2
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 2828
    Number of Blacks = 3
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 2829
    Number of Blacks = 3
    Number of Whites = 0
    What is your best guess?
    Give your guess (a 4-digit number): 2820
    
    Congratulations!!!
    You have found the secret code in just 11 moves!
    Ha! I win!!!
    Do you want to go for another round? (y/n)


## Implementation
The algorithm that we have implemented, in order for the computer to do the guessing of the secret code in a few moves is based on the "[Five-guess algorithm](http://www.cs.uni.edu/~wallingf/teaching/cs3530/resources/knuth-mastermind.pdf)" of Donald Knuth.

## How to compile and execute
In order to compile the code, just download it and use [stack](https://docs.haskellstack.org/en/stable/GUIDE/) like this

    stack build
    
In order to execute it, just run

    stack exec -- mastermind

