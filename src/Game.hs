module Game where

import LevelManager
import Data.Maybe (fromJust)
import Data.Char (toLower, isNumber)

getDirectionByStr :: String -> Maybe Direction
getDirectionByStr []  = Nothing
getDirectionByStr str = let d = toLower . head $ str in
    case d of
        'w' -> Just North
        'd' -> Just East
        's' -> Just South
        'a' -> Just West
        _   -> Nothing

mainGame :: LevelEntry -> IO Bool
mainGame _level = do
    putStrLn $ drawMap _level
    if didPlayerBoxedAllSlots (mapLevelEntry _level) then do
        putStrLn "Victory! Unlocked next map."
        return True
    else do
        raw_dir <- getLine
        if map toLower raw_dir == "k" then do
            putStrLn "Você se matou."
            return False
        else do
            let maybe_dir = getDirectionByStr raw_dir
            case maybe_dir of
                Just dir -> do
                    putStrLn $ "Tentando se mover para: " ++ show dir
                    let new_pos = getPosByDir (posLevelEntry _level) dir
                    let item = getItemByPos new_pos (mapLevelEntry _level)
                    case item of
                        Nothing -> do
                            putStrLn "Funcionou!"
                            mainGame (LevelEntry new_pos (mapLevelEntry _level))
                        Just slot
                                | slot == Box || slot == SlotBox -> do
                                    let new_box_pos = getPosByDir new_pos dir
                                    if isTileXYWalkable new_box_pos (mapLevelEntry _level) then do
                                        putStrLn "Você empurrou a caixa."
                                        mainGame (LevelEntry new_pos (moveBox new_pos new_box_pos (mapLevelEntry _level)))
                                    else do
                                        putStrLn "Você não pode andar por cima de uma caixa!"
                                        mainGame _level
                                | slot == Slot -> do
                                        putStrLn "Você andou em cima de um spot."
                                        mainGame (LevelEntry new_pos (mapLevelEntry _level))
                                | otherwise -> do
                                        putStrLn "Não é possível andar aí."
                                        mainGame _level
                Nothing -> do
                    putStrLn "Direção inválida. Use WASD para andar ou K para suicidar-se."
                    mainGame _level

getNumber :: String -> Maybe Int
getNumber []  = Nothing
getNumber str = if all isNumber str then Just (read str :: Int) else Nothing

mainMenu :: Int -> IO ()
mainMenu fases = do
    putStrLn $ "Fases disponiveis: " ++ show fases ++ " de " ++ show (length levelDatabase)
    putStrLn "> Jogar na fase: "
    raw_fase <- getLine
    let maybe_fase = getNumber raw_fase
    case maybe_fase of
        Nothing -> do
            putStrLn "Numero invalido"
            mainMenu fases
        Just fase -> do if fase `notElem` [1..length levelDatabase] then do
                            putStrLn $ "Fase inexistente. Escolha uma fase entre 1 e " ++ show (length levelDatabase)
                            mainMenu fases
                        else do
                            if fase > fases then do
                                putStrLn "Você ainda não desbloqueou esta fase."
                                mainMenu fases
                            else do
                                putStrLn $ "Iniciando fase " ++ show fase ++ "."
                                win <- mainGame $ fromJust $ getLevelById fase
                                if win then do
                                    putStrLn $ "Você desbloqueou a fase " ++ show (fase + 1) ++ "!"
                                    mainMenu (fases + 1)
                                else do
                                    putStrLn "Você perdeu... tente novamente!"
                                    mainMenu fases

mainStart :: IO ()
mainStart = do
    putStrLn "Init... Use WASD para andar e K para suicidar-se."
    mainMenu 1
