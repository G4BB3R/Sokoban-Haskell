module Game where

import Data.Maybe (fromJust)
import LevelManager
import Data.Char (toLower)

getDirectionByStr :: String -> Maybe Direction
getDirectionByStr []  = Nothing
getDirectionByStr str = let d = toLower . head $ str in
    case d of
        'w' -> Just North
        'd' -> Just East
        's' -> Just South
        'a' -> Just West
        _   -> Nothing

mainGame :: LevelEntry -> IO ()
mainGame _level = do
    putStrLn $ drawMap _level
    raw_dir <- getLine
    let maybe_dir = getDirectionByStr raw_dir
    case maybe_dir of
        Just dir -> do
            putStrLn $ "Trying to move to: " ++ show dir
            let new_pos = getPosByDir (posLevelEntry _level) dir
            let item = getItemByPos new_pos (mapLevelEntry _level)
            case item of
                Nothing -> do
                    putStrLn "Worked!"
                    mainGame (LevelEntry new_pos (mapLevelEntry _level))
                Just slot -> case slot of
                                Slot -> do
                                    putStrLn "Worked! Walked over a spot."
                                    mainGame (LevelEntry new_pos (mapLevelEntry _level))
                                Box  -> do
                                    let new_box_pos = getPosByDir new_pos dir
                                    if isTileXYWalkable new_box_pos (mapLevelEntry _level) then do
                                        putStrLn "You are pushing the box. Great!"
                                        mainGame (LevelEntry new_pos (moveBox new_pos new_box_pos (mapLevelEntry _level)))
                                    else do
                                        putStrLn "You cannot walk over a box!"
                                        mainGame _level
                                _    -> do
                                    putStrLn "Not walkable!"
                                    mainGame _level
        Nothing -> do
            putStrLn "Invalid direction mother fucker XD"
            mainGame _level

mainStart :: IO ()
mainStart = do
    putStrLn "Init... Use WASD to walk."
    mainGame $ fromJust $ getLevelById 1
