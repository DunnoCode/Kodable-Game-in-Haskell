import System.IO

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List
import Data.Maybe

-- stack --resolver=lts setup
-- stack --resolver=lts install random
-- stack --resolver=lts ghci
-- https://stackoverflow.com/questions/42201587/how-to-install-system-random-maybe-cabal-issue-in-version-8-of-stack
import Text.Printf
import System.Directory
import System.Random

-- https://stackoverflow.com/questions/22748546/how-to-add-spaces-to-string-in-haskell
-- addSpace :: String -> String
-- addSpace row = intercalate " " . chunksOf 1

-- https://stackoverflow.com/questions/3098877/remove-white-space-from-string

removeSpace :: [Char] -> [Char]
removeSpace [] =  []
removeSpace (' ': cs) = removeSpace cs
removeSpace (c:cs) = c : removeSpace cs

allRemoveSpace :: [String] -> [String]
allRemoveSpace gameMap = [removeSpace x | x <- gameMap]

-- ************************************** LOAD MAP ********************************* --
-- getFileName function is to let user to input the map if the game does not find the map
-- it will ask the player to enter the name again until the file is found
-- link of teaching the usage of doesFileExist
-- https://programming-idioms.org/idiom/144/check-if-file-exists/2633/haskell
getFileName :: IO String
getFileName = do putStrLn "Please Input Map File Name"
                 fileName <- getLine
                 fileExist <- doesFileExist fileName
                 case fileExist of
                    True -> do putStrLn "Map File Is Found!"
                               return fileName
                    False -> do putStrLn "Map File Does Not Exist!"
                                getFileName

-- link of teaching using readFile
-- http://zvon.org/other/haskell/Outputprelude/readFile_f.html
-- link of teaching the usage of lines to split \n into list
-- https://stackoverflow.com/questions/37656419/how-i-can-split-n-in-haskell
readMap :: String -> IO [String]
readMap fileName = do putStrLn "Reading Map"
                      gameMap <- readFile fileName
                      return $ lines gameMap

-- ************************************** LOAD MAP ********************************* --
-- *************************************Scramble map ******************************* --
buildFile :: [String] -> String -> IO ()
buildFile gameMap fileName = do scrambleFile <- openFile (init(init(init(init fileName))) ++ "_scramble.txt") WriteMode
                                scrambleMap <- scrambleGameMap gameMap []
                                hPutStrLn scrambleFile scrambleMap
                                hClose scrambleFile
                                putStrLn "done!"

scrambleGameMap :: [String] -> String -> IO String
scrambleGameMap gameMap stack = case length gameMap of 
                                    1 -> do stack1 <- scrambleRow (gameMap !! 0) []
                                            let stack2 = stack ++ stack1
                                            return stack2 
                                    _ -> do stack1 <- (scrambleRow (gameMap !! 0) [])
                                            let stack2 = stack ++ stack1 ++ "\n"
                                            scrambleGameMap (tail gameMap) stack2

scrambleRow :: String -> [Char] -> IO String
scrambleRow row stack = case length row of 
                            1 -> do let stack1 = stack ++ (take 1 row)
                                    return stack1
                            _ -> do index <- randomRIO (0, length (row) - 1)
                                    let stack1 = stack ++ [row !! index]
                                    let row1 = take index row ++ drop (1 + index) row
                                    scrambleRow row1 stack1 


-- ************************************* Main function ***************************** --
-- p_ pink y_ yellow o_orange
-- (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down")
playCheck :: [String] -> Int -> Bool
playCheck (x:xs) index
    | singleplay = True
    | checkplay = playCheck xs 1
    | checkFirst = playCheck xs 2
    | checkSecond = playCheck xs 3
    | checkThird = True
    | otherwise = False
    where
        singleplay = (index == 0 && x == "play" && length xs == 0)
        checkplay = (index == 0 && x == "play" && length xs > 0)
        checkFirst = (length xs > 0 && index == 1 && (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down"))
        checkSecond = (length xs > 0 && index == 2 && (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down"))
        checkThird = (length xs == 0 && index == 3 && (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down"))

-- usage of the "words" spliting the command with space "1 2 3 4" -> [1,2,3,4]
-- https://stackoverflow.com/questions/57141325/haskell-string-to-list-string-split-by-whitespace/57142516
getCommand :: IO String
getCommand = do putStrLn "Please Input Command (load/play/quit/check/solve/shuffle/help)"
                command <- getLine
                case (take 4 command) of
                    "load" -> case (length command == 4) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "play" -> case (playCheck (words command) 0) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "quit" -> case (length command == 4) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "chec" -> case (length command == 5) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "solv" -> case (length command == 5) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "shuf" -> case (length command == 7) of
                                True -> return command
                                False -> do putStrLn "Unidentify Command"
                                            getCommand
                    "help" -> do putStrLn "************* --Command Menu-- *************"
                                 putStrLn "load:    Player can input the file name of"
                                 putStrLn "         the game map for the game"
                                 putStrLn "check:   Player can check the map has a "
                                 putStrLn "         solution to solve or not"
                                 putStrLn "solve:   Player can get the answer to solve"
                                 putStrLn "         the map"
                                 putStrLn "play:    Player can run the map to play. If"
                                 putStrLn "         the player needs to use function "
                                 putStrLn "         such that Top Left Right. Player "
                                 putStrLn "         needs to input along with play as"
                                 putStrLn "         well. Function needs to take 3 "
                                 putStrLn "         directions."
                                 putStrLn "         The player will be asked to input"
                                 putStrLn "         the command to run the game map "
                                 putStrLn "         If the player want the ball to "
                                 putStrLn "         stop at color block, then the  "
                                 putStrLn "         player need to set p_, o_, y_"
                                 putStrLn "         on the next direction. Such that"
                                 putStrLn "         Right o_Up the ball will move to"
                                 putStrLn "         right and stop at orange color"
                                 putStrLn "         block, then move up."
                                 putStrLn "         Loop function is along with it"
                                 putStrLn "         player need to input command"
                                 putStrLn "         Loop {max 5} {direction} {direciton}"
                                 putStrLn "shuffle: Player can scramble the existing"
                                 putStrLn "         Game Map"
                                 putStrLn "quit:    Player can quit the game"
                                 putStrLn ""
                                 getCommand
                    _ -> do putStrLn "Unidentify Command"
                            getCommand

-- the usage of mapM_ putStrLn
-- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines



printBoard :: [String] -> IO()
printBoard board = mapM_ putStrLn board

operate :: String -> IO ()
operate fileName = do command <- getCommand
                      case (take 4 command) of
                        "load" -> do newFileName <- getFileName
                                     tempGameMap <- readMap newFileName
                                     let gameMap = allRemoveSpace tempGameMap
                                     case fileName of
                                        "" -> do putStrLn "Read map successfully!"
                                                 putStrLn "Initial:"
                                                 printBoard gameMap
                                        _ -> do putStrLn "New map is updated"
                                                putStrLn "Initial:"
                                                printBoard gameMap
                                     operate newFileName
                        "play" -> do case fileName of
                                        "" -> do putStrLn "PLEASE LOAD THE GAME MAP FIRST"
                                                 operate fileName
                                        _ -> do putStrLn "GAME START"
                                                tempGameMap <- readMap fileName
                                                let gameMap = allRemoveSpace tempGameMap
                                                -- pass the function to the game
                                                -- run gameMap
                                                run gameMap (tail (words command)) fileName
                                                -- TODO
                        "shuf" -> do case fileName of
                                        "" -> do putStrLn "PLEASE LOAD THE GAME MAP FIRST"
                                                 operate fileName
                                        _ -> do putStrLn "Shuffle Map START"
                                                tempGameMap <- readMap fileName
                                                let gameMap = allRemoveSpace tempGameMap
                                                buildFile gameMap fileName
                                                operate fileName
                        "solv" -> do case fileName of
                                        "" -> do putStrLn "PLEASE LOAD THE GAME MAP FIRST"
                                                 operate fileName
                                        _ -> do putStrLn "THE ANSWER:"
                                                tempGameMap <- readMap fileName
                                                let gameMap = allRemoveSpace tempGameMap
                                                -- funciton need for find answer
                                                let gameMap = allRemoveSpace tempGameMap
                                                case (checkMap gameMap) of
                                                     False -> do putStrLn "This map is not solvable.\nPlease load another Game Map."
                                                                 operate fileName
                                                     True -> do let answer = buildCommandfinal gameMap
                                                                putStrLn (intercalate " " answer)
                                                                operate fileName
                                                -- TODO
                                                
                        "chec" -> do case fileName of
                                        "" -> do putStrLn "PLEASE LOAD THE GAME MAP FIRST"
                                                 operate fileName
                                        _ -> do putStrLn "CHECKING"
                                                tempGameMap <- readMap fileName
                                                let gameMap = allRemoveSpace tempGameMap
                                                case (checkMap gameMap) of
                                                     True -> putStrLn "This map is solvable"
                                                     False -> putStrLn "This map is not solvable.\nPlease load another Game Map."
                                                operate fileName
                        "quit" -> return ()
main :: IO ()
main = do putStrLn "WELCOME!!!"
          operate ""

-- ************************************* Main function ***************************** --
-- ************************************* Solving Map ******************************--
checkMap :: [String] -> Bool
checkMap gameMap = checkALLpath (emptyMap gameMap []) (bonusEndList gameMap)
 
checkALLpath :: [String] -> [(Int, Int)] -> Bool
checkALLpath oneMap location = case length location of 
                                   0 -> True
                                   _ -> do let command = shortestPathCommand (addMap oneMap (location !! 0) 'b')
                                           case command of
                                                [] -> False
                                                _ -> checkALLpath oneMap (tail location)
shortestPathAmongAllBonus :: [String] -> [(Int, Int)] -> Int -> Int -> Int -> Int
shortestPathAmongAllBonus emptyGameMap bonusList steps index compare
        | return = index
        | start = shortestPathAmongAllBonus emptyGameMap (tail bonusList) (steps + 1) 0 (length (shortestPathCommand (addMap emptyGameMap (bonusList !! 0) 'b')))
        | continue = shortestPathAmongAllBonus emptyGameMap (tail bonusList) (steps + 1) steps (length (shortestPathCommand (addMap emptyGameMap (bonusList !! 0) 'b')))
        | otherwise = shortestPathAmongAllBonus emptyGameMap (tail bonusList) (steps + 1) index compare
        where 
           return = length bonusList == 0
           start = steps == 0
           continue = compare > (length (shortestPathCommand (addMap emptyGameMap (bonusList !! 0) 'b')))

buildCommand :: [String] -> [(Int, Int)] -> [String] -> Char -> ([String], [String])
buildCommand gameMap bonusList commandlist previous
        | return = (gameMap, commandlist)
        | otherwise = do let empty = emptyMap gameMap []
                         let index = shortestPathAmongAllBonus empty bonusList 0 0 0
                         let updated = addMap empty (bonusList !! index) 'b'
                         let command = shortestPathCommand updated
                         let (runMap, previous1) = changeBall updated command previous
                         buildCommand runMap (take index bonusList ++ drop (1 + index) bonusList) (commandlist ++ command) previous1
        where
            return = length bonusList == 0                 

buildCommandfinal :: [String] -> [String]
buildCommandfinal gameMap = do let bonusList = spotBonus gameMap 0 []
                               let end = spotEnd gameMap (0,0)
                               let (lastMap, command) = buildCommand gameMap bonusList [] '-'
                               let empty = emptyMap lastMap []
                               let updated = addMap empty end 'b'
                               let command1 = shortestPathCommand updated
                               command ++ command1



-- **********************************************************
-- **********************************************************
-- **********************************************************
-- **********************************************************
-- **********************************************************
-- **********************************************************
-- **********************************************************
-- **********************************************************







-- move one steps
changeBall :: [String] -> [String] -> Char -> ([String], Char)
changeBall gameMap command previous = case length command of 
                                           1 -> case ("Right" `isInfixOf` (command !! 0)) of 
                                                    True -> do let map = fst (moveRight gameMap (length (gameMap !! 0)) 0 previous (spotBall gameMap (0,0)))
                                                               let (x,y) = spotBall map (0,0)
                                                               (map, gameMap !! y !! x)
                                                    False -> case ("Up" `isInfixOf` (command !! 0)) of 
                                                                True -> do let map = fst (moveUp gameMap 0 0 previous (spotBall gameMap (0,0)))
                                                                           let (x,y) = spotBall map (0,0)
                                                                           (map, gameMap !! y !! x)
                                                                False -> case ("Down" `isInfixOf` (command !! 0)) of 
                                                                                True -> do let map = fst (moveDown gameMap (length gameMap) 0 previous (spotBall gameMap (0,0)))
                                                                                           let (x,y) = spotBall map (0,0)
                                                                                           (map, gameMap !! y !! x)
                                                                                False -> do let map = fst (moveLeft gameMap 0 0 previous (spotBall gameMap (0,0)))
                                                                                            let (x,y) = spotBall map (0,0)
                                                                                            (map, gameMap !! y !! x)
                                           _ -> case ("Right" `isInfixOf` (command !! 0)) of 
                                                    True -> case (take 2 (command !! 1)) of 
                                                                "o_" -> changeBall (fst (moveRight_O gameMap (length (gameMap !! 0)) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'o'
                                                                "p_" -> changeBall (fst (moveRight_P gameMap (length (gameMap !! 0)) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'p'
                                                                "y_" -> changeBall (fst (moveRight_Y gameMap (length (gameMap !! 0)) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'y'
                                                                _ -> changeBall (fst (moveRight gameMap (length (gameMap !! 0)) 0 previous (spotBall gameMap (0,0)))) (tail command) '-'
                                                    False -> case ("Left" `isInfixOf` (command !! 0)) of
                                                                True -> case (take 2 (command !! 1)) of 
                                                                        "o_" -> changeBall (fst ((moveLeft_O gameMap 0 0 previous (spotBall gameMap (0,0))) 0)) (tail command) 'o'
                                                                        "p_" -> changeBall (fst ((moveLeft_P gameMap 0 0 previous (spotBall gameMap (0,0))) 0)) (tail command) 'p'
                                                                        "y_" -> changeBall (fst ((moveLeft_Y gameMap 0 0 previous (spotBall gameMap (0,0))) 0)) (tail command) 'y'
                                                                        _ -> changeBall (fst ((moveLeft gameMap 0 0 previous (spotBall gameMap (0,0))))) (tail command) '-'
                                                                False -> case ("Up" `isInfixOf` (command !! 0)) of 
                                                                                True -> case (take 2 (command !! 1)) of 
                                                                                        "o_" -> changeBall (fst (moveUp_O gameMap 0 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'o'
                                                                                        "p_" -> changeBall (fst (moveUp_P gameMap 0 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'p'
                                                                                        "y_" -> changeBall (fst (moveUp_Y gameMap 0 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'y'
                                                                                        _ -> changeBall (fst (moveUp gameMap 0 0 previous (spotBall gameMap (0,0)))) (tail command) '-'
                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                        "o_" -> changeBall (fst (moveDown_O gameMap (length gameMap) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'o'
                                                                                        "p_" -> changeBall (fst (moveDown_P gameMap (length gameMap) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'p'
                                                                                        "y_" -> changeBall (fst (moveDown_Y gameMap (length gameMap) 0 previous (spotBall gameMap (0,0)) 0)) (tail command) 'y'
                                                                                        _ -> changeBall (fst (moveDown gameMap (length gameMap) 0 previous (spotBall gameMap (0,0)))) (tail command) '-'
                                        


















-- https://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
replaceRow :: Int -> Char -> String -> String
replaceRow index target row = take index row ++ [target] ++ drop (index + 1) row

replaceColumn :: Int -> String -> [String] -> [String]
replaceColumn index target column = take index column ++ [target] ++ drop (index + 1) column

addMap :: [String] -> (Int, Int) -> Char -> [String]
addMap gameMap (x,y) target = replaceColumn y (replaceRow x target (gameMap !! y)) gameMap 

emptyMap :: [String] -> [String] -> [String]
emptyMap gameMap stack 
   | continue = emptyMap (tail gameMap) (stack ++ [(removeRow (gameMap !! 0) "")])
   | otherwise = stack
   where
     continue = length gameMap > 0 

removeRow :: String -> String -> String
removeRow row stack
   | continue1 = removeRow (tail row) (stack ++ "-")
   | continue2 = removeRow (tail row) (stack ++ (take 1 row)) 
   | continue3 = stack ++ "-"
   | otherwise = stack ++ (take 1 row)
   where 
     continue1 = ((row !! 0 == 'b') || (row !! 0 == 't')) && length row > 1
     continue2 = length row > 1
     continue3 = ((row !! 0 == 'b') || (row !! 0 == 't')) && length row == 1

--spotBonus :: gameMap (start from 0) []
spotBonus :: [String] -> Int -> [(Int, Int)] -> [(Int, Int)]
spotBonus gameMap y stack 
   | continue = spotBonus (tail gameMap) (y + 1) (bonusAtRow (gameMap !! 0) 0 y stack) 
   | otherwise = (bonusAtRow (gameMap !! 0) 0 y stack)
   where
        continue = (length gameMap > 1)
bonusAtRow :: String -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
bonusAtRow row x y stack 
   | addContinue = bonusAtRow (tail row) (x + 1) y (stack ++ [(x, y)])
   | continue = bonusAtRow (tail row) (x + 1) y stack
   | addOtherwise = stack ++ [(x,y)]
   | otherwise = stack
   where 
        addContinue = (row !! 0 == 'b') && (length row > 1) 
        continue = (row !! 0 /= 'b') && (length row > 1)
        addOtherwise = (row !! 0 == 'b') && (length row == 1)

spotEnd :: [String] -> (Int, Int) -> (Int, Int)
spotEnd (gameMap:rest) (x,y)
    | spotted = (fromJust (elemIndex 't' gameMap), y)
    | notSpotted = spotEnd (rest) (x, y + 1)
    | otherwise = (-1, -1)
    where
        spotted = (elem 't' gameMap == True)
        notSpotted = length rest > 0

bonusEndList :: [String] -> [(Int, Int)]
bonusEndList gameMap = spotBonus gameMap 0 [] ++ [spotEnd gameMap (0,0)]


-- shotest path command
shortestPathCommand :: [String] -> [String]
shortestPathCommand gameMap = do let map = (path (gameMap,'-') "")
                                 case length map of 
                                    0 -> []
                                    _ -> do let command = shortest map (map !! 0) 
                                            let command2 = moreShorterPath command [] ""
                                            (take 1 command) ++ command2
-- shortest list2 (list2 !! 0)
-- Right p_Right then change to Right
-- https://www.programming-idioms.org/idiom/39/check-if-string-contains-a-word/858/haskell
moreShorterPath :: [String] -> [String] -> String -> [String]
moreShorterPath command stack previous
    | return = stack
    | continue = moreShorterPath (tail command) stack (command !! 0)
    | otherwise = moreShorterPath (tail command) (stack ++ [command !! 0]) (command !! 0)
    where
        return = length command == 0
        continue = previous `isInfixOf` (command !! 0)

shortest :: [String] -> String -> [String]
shortest input command
    | return = commandConvertor (words (input !!0)) []
    | return1 = commandConvertor (words command) []
    | change = shortest (tail input) (input !! 0)
    | otherwise = shortest (tail input) command
    where
        change = length (words command) > length (words (input !! 0))
        return = length input == 1 && (length (words command) > length (words (input !! 0)))
        return1 = length input == 1

commandConvertor :: [String] -> [String] -> [String]
commandConvertor input stack
    | return = stack
    | remove = commandConvertor (tail input) (stack ++ [tail(tail(input !! 0))])
    | otherwise = commandConvertor (tail input) (stack ++ [input !! 0])
    where
        return = length input == 0 
        remove = take 2 (input !! 0) == "-_" 

path :: ([String], Char) -> String -> [String]
path (gameMap,previous) stack = case (bonusInMap gameMap 0 == 1) of
                                   True -> case (validDown gameMap (length gameMap) (spotBall gameMap (0,0))) of
                                                False -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of 
                                                                False -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                False -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> []
                                                                                                True -> path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up")  -- do Up only
                                                                                True -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")-- do Left only
                                                                                                True -> (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- do Left && Up
                                                                True -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                False -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right") -- do Right only 
                                                                                                True -> (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- Right Up
                                                                                True -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) -- Left Right
                                                                                                True -> (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- Left Up Right
                                                True -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of 
                                                                False -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                False -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")-- do Down only
                                                                                                True -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- do Down and Up 
                                                                                True -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left"))-- do Down Left 
                                                                                                True -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- do Down Left Up
                                                                True -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                False -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) -- Down right 
                                                                                                True -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- Down Right Up
                                                                                True -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                                False -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) ++ (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left"))--Left Right Down 
                                                                                                True -> (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down")) ++ (path (solveRight gameMap (spotBall gameMap (0,0)) (length (gameMap !! 0)) '-') (stack ++ " " ++ [previous] ++ "_Right")) ++ (path (solveLeft gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Left")) ++ (path (solveUp gameMap (spotBall gameMap (0,0)) 0 '-') (stack ++ " " ++ [previous] ++ "_Up"))-- Left up Donw Right
                                   False -> [stack]

-- (path (solveDown gameMap (spotBall gameMap (0,0)) (length gameMap) '-') (stack ++ " " ++ [previous] ++ "_Down"))

solveDown :: [String] -> (Int, Int) -> Int -> Char -> ([String], Char)
solveDown gameMap (ballX, ballY) max condition
   | return_o = (gameMap, condition)
   | return_p = (gameMap, condition)
   | return_y = (gameMap, condition)
   | continue = solveDown (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX '*' (gameMap !! ballY))gameMap)) (ballX, ballY + 1) max (gameMap !! (ballY + 1) !! ballX)
   | otherwise = (gameMap, condition)
   where
        return_o = condition == 'o'
        return_p = condition == 'p'
        return_y = condition == 'y'
        continue = (ballY + 1 /= max) && (gameMap !! (ballY + 1)!! ballX /= '*') 
solveUp :: [String] -> (Int, Int) -> Int -> Char -> ([String], Char)
solveUp gameMap (ballX, ballY) min condition
   | return_o = (gameMap, condition)
   | return_p = (gameMap, condition)
   | return_y = (gameMap, condition)
   | continue = solveUp (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX '*' (gameMap !! ballY))gameMap)) (ballX, ballY - 1) min (gameMap !! (ballY - 1) !! ballX)
   | otherwise = (gameMap, condition)
   where
        return_o = condition == 'o'
        return_p = condition == 'p'
        return_y = condition == 'y'
        continue = (ballY /= min) && (gameMap !! (ballY - 1)!! ballX /= '*') 

solveLeft :: [String] -> (Int, Int) -> Int -> Char -> ([String], Char)
solveLeft gameMap (ballX, ballY) min condition
   | return_o = (gameMap, condition)
   | return_p = (gameMap, condition)
   | return_y = (gameMap, condition)
   | continue = solveLeft (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX '*' (gameMap !! ballY))) gameMap) (ballX - 1, ballY) min (gameMap !! ballY !! (ballX - 1))
   | otherwise = (gameMap, condition)
   where
        return_o = condition == 'o'
        return_p = condition == 'p'
        return_y = condition == 'y'
        continue = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*')

solveRight :: [String] -> (Int, Int) -> Int -> Char -> ([String], Char)
solveRight gameMap (ballX, ballY) max condition
   | return_o = (gameMap, condition)
   | return_p = (gameMap, condition)
   | return_y = (gameMap, condition)
   | continue = solveRight (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX '*' (gameMap !! ballY))) gameMap) (ballX + 1, ballY) max (gameMap !! ballY !! (ballX + 1))
   | otherwise = (gameMap, condition)
   where
        return_o = condition == 'o'
        return_p = condition == 'p'
        return_y = condition == 'y'
        continue = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*')

-- ************************************* GAME function ***************************** --
-- https://stackoverflow.com/questions/4272791/how-to-find-the-index-of-an-element-in-a-list
-- fromJust (elemIndex 9 list)
-- faster than using funciton count 1 by 1
spotBall :: [String] -> (Int, Int) -> (Int, Int)
spotBall (gameMap:rest) (x,y)
    | spotted = (fromJust (elemIndex '@' gameMap), y)
    | notSpotted = spotBall (rest) (x, y + 1)
    | otherwise = (-1, -1)
    where
        spotted = (elem '@' gameMap == True)
        notSpotted = length rest > 0

--- find total number of bonus in game
bonusInRow :: String -> Int
bonusInRow row = length ([x | x <- row, x == 'b'])

bonusInMap :: [String] -> Int -> Int
bonusInMap gameMap number
    | continue = bonusInMap (tail gameMap) (number + bonusInRow (gameMap !! 0))
    | otherwise = number
    where
        continue = length gameMap > 0

--- ready for the stack directions to run on the map
loopCheck :: [String] -> Int -> Bool
loopCheck (x:xs) index
    | checkLoop = loopCheck xs 1
    | checkFirst = loopCheck xs 2
    | checkSecond = loopCheck xs 3
    | checkThird = True
    | otherwise = False
    where
        checkLoop = (index == 0 && x == "Loop" && length xs > 0)
        checkFirst = (length xs > 0 && index == 1 && (read x :: Int) < 6)
        checkSecond = (length xs > 0 && index == 2 && (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down"))
        checkThird = (length xs == 0 && index == 3 && (x == "Left" || x == "p_Left" || x == "y_Left" || x == "o_Left" || x == "Right" || x == "p_Right" || x == "y_Right" || x == "o_Right" || x == "Up" || x == "p_Up" || x == "y_Up" || x == "o_Up" || x == "Down" || x == "p_Down" || x == "y_Down" || x == "o_Down"))

checkDirection :: String -> [String] -> Bool
checkDirection input funciton = case (take 4 input) of
                                        "Loop" -> case (loopCheck (words input) 0) of
                                                        True -> True
                                                        False -> False
                                        _ -> case input of
                                                "Function" -> case funciton of
                                                                [] -> False
                                                                _ -> True
                                                "Left" -> True
                                                "p_Left" -> True
                                                "y_Left" -> True
                                                "o_Left" -> True
                                                "Right" -> True
                                                "p_Right" -> True
                                                "y_Right" -> True
                                                "o_Right" -> True
                                                "Up" -> True
                                                "p_Up" -> True
                                                "y_Up" -> True
                                                "o_Up" -> True
                                                "Down" -> True
                                                "p_Down" -> True
                                                "y_Down" -> True
                                                "o_Down" -> True
                                                _ -> False

getDirection :: [String] -> [String] -> IO [String]
getDirection stack funciton = do putStr "New Direction: "
                                 input <- getLine
                                 if input == ""
                                 then return stack
                                 else
                                     case (checkDirection input funciton) of
                                        True -> getDirection (stack ++ [input]) funciton
                                        False -> do putStrLn "Invalid input"
                                                    getDirection stack funciton
---------------------------------------------------------------------------------------------------------------
------------------------------------------------DO SOLVE-------------------------------------------


--Moving Right to the conditions, in which the ball will stop when it reach the color block

checkWinner :: [String] -> Int -> Int -> Bool
checkWinner gameMap newBonus winbonus
    | return = False
    | return1 = False
    | return2 = True
    | otherwise = checkWinner (tail gameMap) newBonus winbonus
    where
        return = (winbonus > newBonus) == True
        return1 = ('t' `elem` (gameMap !! 0)) == True
        return2 = ('t' `elem` (gameMap !! 0)) == False && (length gameMap == 1) 

printGetBonus :: Int -> Int -> IO ()
printGetBonus newBonus bonus = case (newBonus - bonus > 0) of
                                  True -> do let grab = newBonus - bonus
                                             putStrLn ("You grab " ++ show grab ++ " bonus!")
                                             return ()
                                  False -> return ()


doMove :: [String] -> [String] -> [String] -> Int -> Char -> Int -> IO ()
doMove command function gameMap bonus previous winbonus = case (checkWinner gameMap bonus winbonus) of 
                                                            True -> do putStrLn "You win the game"
                                                                       return ()
                                                            False -> case command !! 0 of
                                                                        "Up" -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus        
                                                                                        False -> do putStrLn "Something goes wrong with Up" 
                                                                                                    return ()
                                                                        "p_Up" -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus        
                                                                                        False -> do putStrLn "Something goes wrong p_Up"
                                                                                                    return ()
                                                                        "y_Up" -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus        
                                                                                        False -> do putStrLn "Something goes wrong y_Up"
                                                                                                    return ()
                                                                        "o_Up" -> case (validUp gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveUp_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveUp_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveUp_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveUp gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus        
                                                                                        False -> do putStrLn "Something goes wrong o_Up"
                                                                                                    return ()
                                                                        "Down" -> case (validDown gameMap (length gameMap) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveDown gameMap (length gameMap) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                        False -> do putStrLn "Something goes wrong Down"
                                                                                                    return ()
                                                                        "p_Down" -> case (validDown gameMap (length gameMap) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveDown gameMap (length gameMap) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                        False -> do putStrLn "Something goes wrong p_Down"
                                                                                                    return ()
                                                                        "y_Down" -> case (validDown gameMap (length gameMap) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveDown gameMap (length gameMap) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                        False -> do putStrLn "Something goes wrong y_Down"
                                                                                                    return ()
                                                                        "o_Down" -> case (validDown gameMap (length gameMap) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveDown gameMap (length gameMap) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board 
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveDown_O gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveDown_P gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveDown_Y gameMap (length gameMap) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        _ -> do let (board, newBonus) = moveDown gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                        False -> do putStrLn "Something goes wrong o_Down"
                                                                                                    return ()
                                                                        "Right" -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus         
                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong Right"
                                                                                                    return ()
                                                                        "p_Right" -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus         
                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong p_Right"
                                                                                                    return ()
                                                                        "o_Right" -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus         
                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong o_Right"
                                                                                                    return ()
                                                                        "y_Right" -> case (validRight gameMap (length (gameMap !! 0)) (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus 
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveRight_O gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveRight_P gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveRight_Y gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus         
                                                                                                        _ -> do let (board, newBonus) = moveRight gameMap (length (gameMap !! 0)) bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong y_Right"
                                                                                                    return ()
                                                                        "Left" -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong Left"
                                                                                                    return ()
                                                                        "p_Left" -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong p_Left"
                                                                                                    return ()
                                                                        "o_Left" -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong o_Left"
                                                                                                    return ()
                                                                        "y_Left" -> case (validLeft gameMap 0 (spotBall gameMap (0,0))) of
                                                                                        True -> case (length command) of        
                                                                                                1 -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                        printGetBonus newBonus bonus
                                                                                                        case (checkWinner board newBonus winbonus) of 
                                                                                                                True -> do printBoard board 
                                                                                                                           putStrLn "You win the game"
                                                                                                                           return ()
                                                                                                                False -> do printBoard board
                                                                                                                            putStrLn "You lose the game"
                                                                                                                            return ()
                                                                                                _ -> case (take 2 (command !! 1)) of 
                                                                                                        "o_" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'o' winbonus
                                                                                                        "p_" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'p' winbonus
                                                                                                        "y_" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                   printGetBonus newBonus bonus
                                                                                                                   printBoard board
                                                                                                                   doMove (tail command) function board newBonus 'y' winbonus
                                                                                                        "Fu" -> case (take 1 (function) !! 0) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                                        "Lo" -> case (take 1 (words (command !! 1)) !! 2) of 
                                                                                                                        "o" -> do let (board, newBonus) = moveLeft_O gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'o' winbonus
                                                                                                                        "p" -> do let (board, newBonus) = moveLeft_P gameMap 0 bonus previous (spotBall gameMap (0,0)) 0
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'p' winbonus
                                                                                                                        "y" -> do let (board, newBonus) = moveLeft_Y gameMap 0 bonus previous (spotBall gameMap (0,0)) 0 
                                                                                                                                  printGetBonus newBonus bonus
                                                                                                                                  printBoard board
                                                                                                                                  doMove (tail command) function board newBonus 'y' winbonus
                                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                                printGetBonus newBonus bonus
                                                                                                                                printBoard board
                                                                                                                                doMove (tail command) function board newBonus '-' winbonus          
                                                                                                        _ -> do let (board, newBonus) = moveLeft gameMap 0 bonus previous (spotBall gameMap (0,0))
                                                                                                                printGetBonus newBonus bonus
                                                                                                                printBoard board
                                                                                                                doMove (tail command) function board newBonus '-' winbonus
                                                                                        False -> do putStrLn "Something goes wrong y_Left"
                                                                                                    return ()
                                                                        "Function" -> moveFunction command function gameMap (function !! 0) (function !! 1) (function !! 2) bonus previous winbonus
                                                                        _ -> moveLoop command function gameMap (words (command !! 0) !! 2) (words (command !! 0) !! 3) bonus previous winbonus (read (words (command !! 0) !! 1) :: Int) 0

moveLoop :: [String] -> [String] -> [String] -> String -> String -> Int -> Char -> Int -> Int -> Int -> IO ()
moveLoop command function gameMap first second bonus previous winbonus time start
        | loopStart = moveLoop (tail command) function gameMap first second bonus previous winbonus time 1
        | appendCommand = moveLoop ([first] ++ [second] ++ command) function gameMap first second bonus previous winbonus (time - 1) start
        | otherwise = doMove command function gameMap bonus previous winbonus
        where 
           loopStart = start == 0
           appendCommand = start == 1 && time /= 0

moveFunction :: [String] -> [String] -> [String] -> String -> String -> String -> Int -> Char -> Int -> IO ()
moveFunction command function gameMap first second third bonus previous winbonus = doMove ([first] ++ [second] ++ [third] ++ tail command) function gameMap bonus previous winbonus

moveRight_P :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveRight_P gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveRight_P (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continueOne = moveRight_P (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | continueBonus = moveRight_P (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continue = moveRight_P (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b') && start == 0
        continueOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && start == 0
        continueBonus = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b') && start == 1
        continue = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && (previous /= 'p') && start == 1
moveRight_O :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveRight_O gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveRight_O (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continueOne = moveRight_O (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | continueBonus = moveRight_O (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continue = moveRight_O (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b') && start == 0
        continueOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && start == 0
        continueBonus = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b')
        continue = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && (previous /= 'o') && start == 1
moveRight_Y :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveRight_Y gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveRight_Y (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continueOne = moveRight_Y (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | continueBonus = moveRight_Y (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY) 1
   | continue = moveRight_Y (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b') && start == 0
        continueOne = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && start == 0
        continueBonus = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b')
        continue = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && (previous /= 'y') && start == 1

--Moving to Right
moveRight :: [String] -> Int -> Int -> Char -> (Int, Int) -> ([String], Int)
moveRight gameMap max bonus previous (ballX, ballY)
   | continueBonus = moveRight (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max (bonus + 1) '-' (ballX + 1, ballY)
   | continue = moveRight (replaceColumn ballY (replaceRow (ballX + 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) max bonus (gameMap !! ballY !! (ballX + 1)) (ballX + 1, ballY)
   | otherwise = (gameMap, bonus)
   where
        continueBonus = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*') && ((gameMap !! ballY !! (ballX + 1)) == 'b')
        continue = (ballX + 1 /= max) && (gameMap !! ballY !! (ballX + 1) /= '*')
--Moving to Left
moveLeft :: [String] -> Int -> Int -> Char -> (Int, Int) -> ([String], Int)
moveLeft gameMap min bonus previous (ballX, ballY)
   | continueBonus = moveLeft (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY)
   | continue = moveLeft (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY)
   | otherwise = (gameMap, bonus)
   where
        continueBonus = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b')
        continue = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*')

moveLeft_Y :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveLeft_Y gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveLeft_Y (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1
   | continueOne = moveLeft_Y (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1
   | continueBonus = moveLeft_Y (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1 
   | continue = moveLeft_Y (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1 
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 0
        continueOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && start == 0
        continueBonus = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 1
        continue = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && (previous /= 'y') && (start == 1)

moveLeft_O :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveLeft_O gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveLeft_O (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1
   | continueOne = moveLeft_O (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1
   | continueBonus = moveLeft_O (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1 
   | continue = moveLeft_O (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 0
        continueOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && start == 0
        continueBonus = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 1
        continue = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && (previous /= 'o') && (start == 1)

moveLeft_P :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveLeft_P gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveLeft_P (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1
   | continueOne = moveLeft_P (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1
   | continueBonus = moveLeft_P (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min (bonus + 1) '-' (ballX - 1, ballY) 1 
   | continue = moveLeft_P (replaceColumn ballY (replaceRow (ballX - 1) '@' (replaceRow ballX previous (gameMap !! ballY))) gameMap) min bonus (gameMap !! ballY !! (ballX - 1)) (ballX - 1, ballY) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 0
        continueOne = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && start == 0
        continueBonus = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && ((gameMap !! ballY !! (ballX - 1)) == 'b') && start == 1
        continue = (ballX /= min) && (gameMap !! ballY !! (ballX - 1) /= '*') && (previous /= 'p') && (start == 1)


--Moving to Up
moveUp :: [String] -> Int -> Int -> Char -> (Int, Int) -> ([String], Int)
moveUp gameMap min bonus previous (ballX, ballY)
   | continueBonus = moveUp (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1)
   | continue = moveUp (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1)
   | otherwise = (gameMap, bonus)
   where
        continueBonus = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b')
        continue = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*')

moveUp_Y :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveUp_Y gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveUp_Y (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continueOne = moveUp_Y (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | continueBonus = moveUp_Y (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continue = moveUp_Y (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b') && start == 0
        continueOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b')
        continue = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (previous /= 'y') && (start == 1)

moveUp_O :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveUp_O gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveUp_O (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continueOne = moveUp_O (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | continueBonus = moveUp_O (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continue = moveUp_O (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b') && start == 0
        continueOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b')
        continue = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (previous /= 'o') && (start == 1)

moveUp_P :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveUp_P gameMap min bonus previous (ballX, ballY) start
   | continueBonusOne = moveUp_P (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continueOne = moveUp_P (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | continueBonus = moveUp_P (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min (bonus + 1) '-' (ballX, ballY - 1) 1 
   | continue = moveUp_P (replaceColumn (ballY - 1) (replaceRow ballX '@' (gameMap !! (ballY - 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) min bonus (gameMap !! (ballY - 1) !! ballX) (ballX, ballY - 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b') && start == 0
        continueOne = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (gameMap !! (ballY - 1) !! ballX == 'b')
        continue = (ballY /= min) && (gameMap !! (ballY - 1) !! ballX /= '*') && (previous /= 'p') && (start == 1)
--Moving to Down
moveDown :: [String] -> Int -> Int -> Char -> (Int, Int) -> ([String], Int)
moveDown gameMap max bonus previous (ballX, ballY)
   | continueBonus = moveDown (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1)
   | continue = moveDown (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1)
   | otherwise = (gameMap, bonus)
   where
        continueBonus = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b')
        continue = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*')

moveDown_Y :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveDown_Y gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveDown_Y (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1 
   | continueOne = moveDown_Y (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | continueBonus = moveDown_Y (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1 
   | continue = moveDown_Y (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b') && start == 0
        continueOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b')
        continue = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (previous /= 'y') && (start == 1)

moveDown_O :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveDown_O gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveDown_O (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1 
   | continueOne = moveDown_O (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | continueBonus = moveDown_O (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1 
   | continue = moveDown_O (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b') && start == 0
        continueOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b')
        continue = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (previous /= 'o') && (start == 1)

moveDown_P :: [String] -> Int -> Int -> Char -> (Int, Int) -> Int -> ([String], Int)
moveDown_P gameMap max bonus previous (ballX, ballY) start
   | continueBonusOne = moveDown_P (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1
   | continueOne = moveDown_P (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | continueBonus = moveDown_P (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max (bonus + 1) '-' (ballX, ballY + 1) 1
   | continue = moveDown_P (replaceColumn (ballY + 1) (replaceRow ballX '@' (gameMap !! (ballY + 1))) (replaceColumn ballY (replaceRow ballX previous (gameMap !! ballY))gameMap)) max bonus (gameMap !! (ballY + 1) !! ballX) (ballX, ballY + 1) 1
   | otherwise = (gameMap, bonus)
   where
        continueBonusOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b') && start == 0
        continueOne = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && start == 0
        continueBonus = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (gameMap !! (ballY + 1) !! ballX == 'b')
        continue = (ballY + 1/= max) && (gameMap !! (ballY + 1) !! ballX /= '*') && (previous /= 'p') && (start == 1)
        



validUp :: [String] -> Int -> (Int, Int) -> Bool
validUp gameMap min (ballX, ballY)
   | return1 = False
   | return2 = False
   | otherwise = True
   where
        return1 = (ballY == min)
        return2 = (gameMap !! (ballY - 1) !! ballX == '*')

validDown :: [String] -> Int -> (Int, Int) -> Bool
validDown gameMap max (ballX, ballY)
   | return1 = False
   | return2 = False
   | otherwise = True
   where
        return1 = (ballY + 1 == max)
        return2 = (gameMap !! (ballY + 1) !! ballX == '*')

validRight :: [String] -> Int -> (Int, Int) -> Bool
validRight gameMap max (ballX, ballY)
   | return1 = False
   | return2 = False
   | otherwise = True
   where
        return1 = (ballX + 1 == max)
        return2 = (gameMap !! ballY !! (ballX + 1) == '*')

validLeft :: [String] -> Int -> (Int, Int) -> Bool
validLeft gameMap min (ballX, ballY)
   | return1 = False
   | return2 = False
   | otherwise = True
   where
        return1 = (ballX == min)
        return2 = (gameMap !! ballY !! (ballX - 1) == '*')

-- validFunction :: [String] -> [String] -> (Int, Int) -> Bool
-- validFunction gameMap funciton (ballX, ballY) = do let (x,y) = mapSize gameMap

mapSize :: [String] -> (Int, Int)
mapSize gameMap = (length (gameMap !! 0), length gameMap)

-- convert tuple to string
locationShow :: (Int, Int) -> String
locationShow (x,y) = show x ++ "" ++ show y


run :: [String] -> [String] -> String -> IO ()
run gameMap function fileName = do printBoard gameMap
                                   directions <- getDirection [] function
                                   let bonus = bonusInMap gameMap 0
                                   let ballLocation = spotBall gameMap (0,0)
                                   let mapsize = mapSize gameMap
                                   printBoard directions
                                   case length directions of 
                                        0 -> do putStrLn "Don't have any actual command for the ball to move"
                                                operate fileName
                                        _ -> do doMove directions function gameMap 0 '-' (bonusInMap gameMap 0)
                                                operate fileName
-- ************************************* GAME function ***************************** --

