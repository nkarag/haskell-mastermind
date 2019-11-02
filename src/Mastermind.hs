{-#  LANGUAGE 
     OverloadedStrings 
    ,BangPatterns   
#-}

-- see:
-- https://puzzling.stackexchange.com/questions/546/clever-ways-to-solve-mastermind

module Mastermind
    ( 
        Code
        ,computerDoestheGuessing
        ,humanDoestheGuessing
        ,getGuessResult
        ,generateSecretCode
        ,answer2code
        ,computerDoestheGuessingHard
        ,humanDoestheGuessingHard
        ,getGuessResultHard
        ,generateSecretCodeHard
        ,answer2codeHard        
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, sort, group)
import System.Random
import System.IO
import Data.Char (digitToInt)
import Data.Char (intToDigit)
import Data.Char (chr)
-- import Debug.Trace

-- | A mastermind secret code
type Code = (Int, Int, Int, Int)

type CharCode = (Char, Char, Char, Char)

-- | Result after verifying a guess against the secret code
data Result = Result Int -- ^ #whites
                     Int  -- ^ #blacks
                     deriving (Eq, Show)

printCode :: Code -> String 
printCode (d1,d2,d3,d4) = (show d1) ++ (show d2) ++ (show d3) ++ (show d4)

printCharCode :: CharCode -> String
printCharCode (d1,d2,d3,d4) = (show d1) ++ (show d2) ++ (show d3) ++ (show d4)
    
-- | Plays mastermind with computer choosing the secret code
computerDoestheGuessing :: IO(Int) -- ^ returns number of moves until secret code is found
computerDoestheGuessing = do
    
    let 
        -- build list of candidate codes
        initLsOfCandidateCodes = [(d1::Int ,d2::Int ,d3::Int ,d4::Int) | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9], d4 <- [0..9]]
        numOfMoves = 1::Int
        initialGuess = (1::Int ,1::Int ,2::Int ,2::Int)
    finalNumOfMoves <- goloop1 initLsOfCandidateCodes numOfMoves initialGuess    
    return finalNumOfMoves

    where
        goloop1 :: [Code] -> Int -> Code -> IO(Int)
        goloop1 candcodes moves guess = do

            
            -- debug
            {-print candcodes           
            putStrLn $ "Debug: Length of candidate list = " ++ (show $ length candcodes)
            putStrLn $ "Debug: Num of moves = " ++ (show moves)-}
            

            -- verify code with human
            putStrLn $ "Is it " ++ (printCode guess) ++ "?"
            putStrLn $ "How many Blacks? (0 - 4)"
            ansBlacks <- getLine
            let numBlacks = (read ansBlacks)::Int
            if numBlacks < 0 || numBlacks > 4
                then do
                        putStrLn ""
                        putStrLn "Wrong input! Please try again!"
                        putStrLn ""
                        goloop1 candcodes moves guess
                else
                    do 

                        if numBlacks == 4 
                            then
                                return moves
                            else 
                                do
                                    putStrLn "thank you!"
                                    putStrLn ""

                                    putStrLn $ "How many Whites?"
                                    ansWhites <- getLine
                                    let numWhites = (read ansWhites)::Int
                                    if numBlacks + numWhites > 4 || numWhites < 0 || (numWhites == 1 && numBlacks == 3)
                                        then do
                                                putStrLn ""
                                                putStrLn "Wrong input! Please try again!"
                                                putStrLn ""
                                                goloop1 candcodes moves guess
                                        else
                                            
                                            -- if correct then print the code and return the #moves
                                            if numBlacks == 4 
                                                then 
                                                    return moves
                                            else
                                                do 
                                                    -- eliminate impossible codes from candidate list
                                                    let 
                                                        newcandcodes = elimImpossibleCodes candcodes guess $ Result numWhites numBlacks
                                                        -- get a new candidate code
                                                        newguess = head newcandcodes
                                                        -- loop again
                                                    goloop1 newcandcodes (moves + 1) newguess

        -- Check each code in the candidate list against the guess (as if this was the secret code)
        -- Filter out all codes in the list who return a result different than the input result (numOfWhites, numOfBlacks)
        elimImpossibleCodes :: [Code] -> Code -> Result -> [Code]
        elimImpossibleCodes codes guess res = 
            -- keep only those candidate codes that return the same result (with the original secret code) when checked against the same guess
            filter (\c -> (getGuessResult guess c) == res) codes

-- Check a guess against a code and returns the result
getGuessResult :: 
           Code  -- ^ guess code
        -> Code  -- ^ secret code
        -> Result
getGuessResult (g1,g2,g3,g4) (s1,s2,s3,s4) = 
    let 
        gList = [g1,g2,g3,g4]
        sList = [s1,s2,s3,s4]
        numOfBlacks = sum $ zipWith (\g s -> if g == s then 1::Int else 0::Int) gList sList

        {-numOfWhites = sum $ map ( \g -> sum $
                                                map ( \s -> if (s == g) && ( (fromJust $ elemIndex s sList) /= (fromJust $ elemIndex g gList) ) -- same value but different position
                                                                then 1::Int
                                                                else 0::Int
                                                    ) sList
                                ) gList-}
        
        --Algorithm how to find the number of whites:

        -- Step1: get rid of digits that correspond to blacks in both lists
        zippedList = filter (\(g,s) -> g /= s) (zip gList sList)
        new_gList = fst $ unzip zippedList
        new_sList = snd $ unzip zippedList

        -- Step2: Now that blacks have gone, we dont need to check position equality any more
        -- However we need to eliminate dublicates from the new guess list, in order not to count multiple whites.
        -- In order to achieve this: group by each list so as to produce two new lists likes this:
        --      gListGrouped = [(g1, cnt1) (g2, cnt2) (g3, cnt3), (g4, cnt4)]
        --      sListGrouped = [(s1, cnt1) (s2, cnt2) (s3, cnt3), (s4, cnt4)]
        gListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_gList)  -- note that the Data.List.group function produces a list of lists: group :: Eq a => [a] -> [[a]]
        sListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_sList)

        -- Step 3: Traverse the grouped sList for each element of the grouped gList
        --  So now we have eliminated dublicates without loosing the information of how many occurences we have for each digit.
        --  For each element gi of gListGrouped we do:
        --      We traverse sListGrouped:
        --          if we find gi == si then we have to check the counters and return the minimum one, i.e, return : min (cnt(gi) , cnt(si))
        --          else (gi =/ si) return : 0
        numOfWhites =   sum $ map (
                                    -- for each guess code digit return the number of whites
                                    \(g, numOfg) -> sum $
                                        -- traverse the secret code list
                                        map (
                                            \(s,numOfs) -> if s == g then min (numOfg::Int) (numOfs::Int) else 0::Int
                                        ) sListGrouped

                                ) gListGrouped          
    in Result numOfWhites numOfBlacks

-- | Plays mastermind with human choosing the secret code
humanDoestheGuessing :: StdGen -> IO(Int) -- ^ returns number of moves until secret code is found
humanDoestheGuessing gen = do -- undefined
    
    -- generate a secret code
    -- gen <- getStdGen
    let scode = generateSecretCode gen

    -- debug
    -- putStrLn $ "Secret Code = " ++ (printCode scode)


    putStrLn "\nOK. Lets start!\n"

    let numOfMoves = 1::Int
    finalNumOfMoves <- goloop2 numOfMoves scode    
    return finalNumOfMoves    

    where
        goloop2 :: Int -> Code -> IO(Int)
        goloop2 moves scode = do 
            putStrLn "What is your best guess?"
            putStr "Give your guess (a 4-digit number): "
            hFlush stdout
            guess <- getLine
            let 
                gcode = answer2code guess
                Result numOfWhites numOfBlacks = getGuessResult gcode scode
            if numOfBlacks == 4 && numOfWhites == 0 
                then 
                    return moves
                else
                    do 
                        putStrLn $ "Number of Blacks = " ++ (show numOfBlacks)
                        putStrLn $ "Number of Whites = " ++ (show numOfWhites)
                        goloop2 (moves + 1) scode

generateSecretCode :: StdGen -> Code
generateSecretCode gen = 
    let
        ls = take 4 (randoms gen :: [Int])        
        [s1,s2,s3,s4] = map (\s -> abs $ mod s 10 ) ls
    in (s1,s2,s3,s4) 

answer2code :: String -> Code
answer2code ans = 
    let
        -- get prefix
        [c1,c2,c3,c4] = take 4 ans
    in (digitToInt c1, digitToInt c2, digitToInt c3, digitToInt c4)


--------------------------- Hard Mode ------------------------------------ 

-- | Plays mastermind with computer choosing the secret code
computerDoestheGuessingHard :: IO(Int) -- ^ returns number of moves until secret code is found
computerDoestheGuessingHard = do
    
    let 
        alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
        -- build list of candidate codes
        initLsOfCandidateCodes = [(d1::Char ,d2::Char ,d3::Char ,d4::Char) | d1 <- alphabet, d2 <- alphabet, d3 <- alphabet, d4 <- alphabet]
        numOfMoves = 1::Int
        initialGuess = ('a'::Char ,'b'::Char ,'c'::Char ,'d'::Char)
    finalNumOfMoves <- goloop1 initLsOfCandidateCodes numOfMoves initialGuess 
    return finalNumOfMoves

    where
        goloop1 :: [CharCode] -> Int -> CharCode -> IO(Int)
        goloop1 candcodes moves guess = do

            
            -- debug
            {-print candcodes           
            putStrLn $ "Debug: Length of candidate list = " ++ (show $ length candcodes)
            putStrLn $ "Debug: Num of moves = " ++ (show moves)-}
            

            -- verify code with human
            putStrLn $ "Is it " ++ (printCharCode guess) ++ "?"
            putStrLn $ "How many Blacks? (0 - 4)"
            ansBlacks <- getLine
            let numBlacks = (read ansBlacks)::Int
            if numBlacks < 0 || numBlacks > 4
                then do
                        putStrLn ""
                        putStrLn "Wrong input! Please try again!"
                        putStrLn ""
                        goloop1 candcodes moves guess
                else
                    do 

                        if numBlacks == 4 
                            then
                                return moves
                            else 
                                do
                                    putStrLn "thank you!"
                                    putStrLn ""

                                    putStrLn $ "How many Whites?"
                                    ansWhites <- getLine
                                    let numWhites = (read ansWhites)::Int
                                    if numBlacks + numWhites > 4 || numWhites < 0 || (numWhites == 1 && numBlacks == 3)
                                        then do
                                                putStrLn ""
                                                putStrLn "Wrong input! Please try again!"
                                                putStrLn ""
                                                goloop1 candcodes moves guess
                                        else
                                            
                                            -- if correct then print the code and return the #moves
                                            if numBlacks == 4 
                                                then 
                                                    return moves
                                            else
                                                do 
                                                    -- eliminate impossible codes from candidate list
                                                    let 
                                                        newcandcodes = elimImpossibleCodes candcodes guess $ Result numWhites numBlacks
                                                        -- get a new candidate code
                                                        newguess = head newcandcodes
                                                        -- loop again
                                                    goloop1 newcandcodes (moves + 1) newguess

        -- Check each code in the candidate list against the guess (as if this was the secret code)
        -- Filter out all codes in the list who return a result different than the input result (numOfWhites, numOfBlacks)
        elimImpossibleCodes :: [CharCode] -> CharCode -> Result -> [CharCode]
        elimImpossibleCodes codes guess res = 
            -- keep only those candidate codes that return the same result (with the original secret code) when checked against the same guess
            filter (\c -> (getGuessResultHard guess c) == res) codes

-- Check a guess against a code and returns the result
getGuessResultHard :: 
           CharCode  -- ^ guess code
        -> CharCode  -- ^ secret code
        -> Result
getGuessResultHard (g1,g2,g3,g4) (s1,s2,s3,s4) = 
    let 
        gList = [g1,g2,g3,g4]
        sList = [s1,s2,s3,s4]
        numOfBlacks = sum $ zipWith (\g s -> if g == s then 1::Int else 0::Int) gList sList

        {-numOfWhites = sum $ map ( \g -> sum $
                                                map ( \s -> if (s == g) && ( (fromJust $ elemIndex s sList) /= (fromJust $ elemIndex g gList) ) -- same value but different position
                                                                then 1::Int
                                                                else 0::Int
                                                    ) sList
                                ) gList-}
        
        --Algorithm how to find the number of whites:

        -- Step1: get rid of digits that correspond to blacks in both lists
        zippedList = filter (\(g,s) -> g /= s) (zip gList sList)
        new_gList = fst $ unzip zippedList
        new_sList = snd $ unzip zippedList

        -- Step2: Now that blacks have gone, we dont need to check position equality any more
        -- However we need to eliminate dublicates from the new guess list, in order not to count multiple whites.
        -- In order to achieve this: group by each list so as to produce two new lists likes this:
        --      gListGrouped = [(g1, cnt1) (g2, cnt2) (g3, cnt3), (g4, cnt4)]
        --      sListGrouped = [(s1, cnt1) (s2, cnt2) (s3, cnt3), (s4, cnt4)]
        gListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_gList)  -- note that the Data.List.group function produces a list of lists: group :: Eq a => [a] -> [[a]]
        sListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_sList)

        -- Step 3: Traverse the grouped sList for each element of the grouped gList
        --  So now we have eliminated dublicates without loosing the information of how many occurences we have for each digit.
        --  For each element gi of gListGrouped we do:
        --      We traverse sListGrouped:
        --          if we find gi == si then we have to check the counters and return the minimum one, i.e, return : min (cnt(gi) , cnt(si))
        --          else (gi =/ si) return : 0
        numOfWhites =   sum $ map (
                                    -- for each guess code digit return the number of whites
                                    \(g, numOfg) -> sum $
                                        -- traverse the secret code list
                                        map (
                                            \(s,numOfs) -> if s == g then min (numOfg::Int) (numOfs::Int) else 0::Int
                                        ) sListGrouped

                                ) gListGrouped          
    in Result numOfWhites numOfBlacks

-- | Plays mastermind with human choosing the secret code
humanDoestheGuessingHard :: StdGen -> IO(Int) -- ^ returns number of moves until secret code is found
humanDoestheGuessingHard gen = do -- undefined
    
    -- generate a secret code
    -- gen <- getStdGen
    let scode = generateSecretCodeHard gen

    -- debug
    -- putStrLn $ "Secret Code = " ++ (printCode scode)


    putStrLn "\nOK. Lets start!\n"

    let numOfMoves = 1::Int
    finalNumOfMoves <- goloop2 numOfMoves scode    
    return finalNumOfMoves    

    where
        goloop2 :: Int -> CharCode -> IO(Int)
        goloop2 moves scode = do 
            putStrLn "What is your best guess?"
            putStr "Give your guess (a 4 digit lowercaps letter combination, ex. adbu): "
            hFlush stdout
            guess <- getLine
            let 
                gcode = answer2codeHard guess
                Result numOfWhites numOfBlacks = getGuessResultHard gcode scode
            if numOfBlacks == 4 && numOfWhites == 0 
                then 
                    return moves
                else
                    do 
                        putStrLn $ "Number of Blacks = " ++ (show numOfBlacks)
                        putStrLn $ "Number of Whites = " ++ (show numOfWhites)
                        goloop2 (moves + 1) scode

generateSecretCodeHard :: StdGen -> CharCode
generateSecretCodeHard gen = 
    let
        ls = take 4 (randoms gen :: [Int])        
        [s1,s2,s3,s4] = map (\s -> abs $ mod s 25 ) ls
    in (chr (s1 + 97), chr (s2 + 97), chr (s3 + 97), chr (s4 + 97))

answer2codeHard :: String -> CharCode
answer2codeHard ans = 
    let
        -- get prefix
        [c1,c2,c3,c4] = take 4 ans
    in (c1,c2,c3,c4)
