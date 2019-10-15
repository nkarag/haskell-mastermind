module Main where

import Mastermind
import System.Random

main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn "Lets play a game of Mastermind!"
    putStrLn "Please choose a difficulty level: Easy [E] or Hard [H]"
    difficulty <- getLine
    if difficulty == "E"
        then  
            do
            gen <- getStdGen
            playGame gen 
        else
            do
            gen <- getStdGen
            playGameHard gen

playGameHard :: StdGen -> IO()
playGameHard gen = do
    putStrLn "You chose the difficulty level: Hard"
    putStrLn "You go first.."
    putStrLn "Choose your secret code (a 4 digit lowercaps letter combination, ex. adbu)"
    putStrLn "Press <enter> when you are ready"
    _ <- getLine
    numOfMovesComputer <- computerDoestheGuessingHard
    putStrLn "\n!!!!!!!!!!!!!!!!!!\n"
    putStrLn $ "Great! I have found the secret code in " ++ (show numOfMovesComputer) ++ " moves!"
    putStrLn "Am I an awesome player or what?"
    putStrLn ""

    putStrLn "OK. Now its your turn!"
    putStrLn "I will choose a secret code."
    putStrLn "Press <enter> when you are ready"
    _ <- getLine 
    -- get a random generator   
    -- gen <- getStdGen
    let 
        (_, genNew) = (random gen) :: (Int, StdGen)
        -- genNew = snd $ random gen 
    numOfMovesHuman <- humanDoestheGuessingHard genNew
    putStrLn $ "\nCongratulations!!!\nYou have found the secret code in just " ++ (show numOfMovesHuman) ++ " moves!"

    if numOfMovesComputer < numOfMovesHuman
        then putStrLn "Ha! I win!!!"
        else 
            if numOfMovesComputer == numOfMovesHuman
                then putStrLn "We have a tie!"
                else putStrLn "Congrats! You Win!"

    putStrLn "Do you want to go for another round? (y/n)"
    ans <- getLine
    if ans == "y"
        then playGame genNew
        else
            do  
                putStrLn "Bye!"
                return ()


playGame :: StdGen -> IO()
playGame gen = do
    putStrLn "You chose the difficulty level: Easy"
    putStrLn "You go first..."
    putStrLn "Choose your secret code (a 4 digit number)"
    putStrLn "Press <enter> when you are ready"
    _ <- getLine
    numOfMovesComputer <- computerDoestheGuessing
    putStrLn "\n!!!!!!!!!!!!!!!!!!\n"
    putStrLn $ "Great! I have found the secret code in " ++ (show numOfMovesComputer) ++ " moves!"
    putStrLn "Am I an awesome player or what?"
    putStrLn ""

    putStrLn "OK. Now its your turn!"
    putStrLn "I will choose a secret code."
    putStrLn "Press <enter> when you are ready"
    _ <- getLine 
    -- get a random generator   
    -- gen <- getStdGen
    let 
        (_, genNew) = (random gen) :: (Int, StdGen)
        -- genNew = snd $ random gen 
    numOfMovesHuman <- humanDoestheGuessing genNew
    putStrLn $ "\nCongratulations!!!\nYou have found the secret code in just " ++ (show numOfMovesHuman) ++ " moves!"

    if numOfMovesComputer < numOfMovesHuman
        then putStrLn "Ha! I win!!!"
        else 
            if numOfMovesComputer == numOfMovesHuman
                then putStrLn "We have a tie!"
                else putStrLn "Congrats! You Win!"

    putStrLn "Do you want to go for another round? (y/n)"
    ans <- getLine
    if ans == "y"
        then playGame genNew
        else
            do  
                putStrLn "Bye!"
                return ()

