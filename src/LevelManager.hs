module LevelManager where
import Data.Maybe (isNothing)

data Item  = Vacuo | Wall | Slot | Box | SlotBox deriving (Eq)
data Tile  = Tile { posTile :: Pos, posItem :: Maybe Item }
type Map   = [[Tile]]
type Pos   = (Int, Int)
type Boxes = [Pos]
data LevelEntry = LevelEntry { posLevelEntry :: Pos, mapLevelEntry :: Map }
data Direction  = North | East | South | West deriving (Show)

getLevelById :: Int -> Maybe LevelEntry
getLevelById _id = if _id `elem` [1..length levelDatabase] then Just (levelDatabase !! (_id - 1)) else Nothing

isPosEquals :: Pos -> Pos -> Bool
isPosEquals posA posB = fst posA == fst posB && snd posA == snd posB

didPlayerBoxedAllSlots :: Map -> Bool
didPlayerBoxedAllSlots _map = Just Slot `notElem` [posItem x | x <- concat _map] 

charToTile :: Char -> Maybe Item
charToTile '.' = Just Vacuo
charToTile 'X' = Just Wall
charToTile 'x' = Just Wall
charToTile 'º' = Just Slot
charToTile 'B' = Just Box
charToTile 'b' = Just Box
charToTile 'S' = Just SlotBox
charToTile 's' = Just SlotBox
charToTile  _  = Nothing

mapToPosifiedMap :: [[Maybe Item]] -> Map
mapToPosifiedMap _map = map (\(x, xs_tuple_y_and_value) ->
                            map (\(y, value) ->
                                    Tile (y, x) value
                                ) xs_tuple_y_and_value
                            ) (zip [1..] (map (zip [1..]) _map))

getPosByDir :: Pos -> Direction -> Pos
getPosByDir (x, y) North = (x + 0, y - 1)
getPosByDir (x, y)  East = (x + 1, y + 0)
getPosByDir (x, y) South = (x + 0, y + 1)
getPosByDir (x, y)  West = (x - 1, y + 0)

isTileWalkable :: Tile -> Bool
isTileWalkable tile = case posItem tile of
                        Just tile' -> tile' == Slot
                        Nothing    -> True

isTileXYWalkable :: Pos -> Map -> Bool
isTileXYWalkable (x, y) _map = isTileWalkable (_map !! (y - 1) !! (x - 1))

getItemByPos :: Pos -> Map -> Maybe Item
getItemByPos (x, y) _map = posItem (_map !! (y - 1) !! (x - 1))

strToMap :: [String] -> Map
strToMap = mapToPosifiedMap . map (map charToTile)

tileToChar :: Pos -> Tile -> Char
tileToChar pos tile = if isPosEquals pos (posTile tile) then 'λ' else
    case posItem tile of
                    Nothing    -> ' '
                    Just tile' -> case tile' of
                                    Vacuo   -> '.'
                                    Wall    -> 'X'
                                    Slot    -> '_'
                                    SlotBox -> 's'
                                    Box     -> 'o'

moveBox :: Pos -> Pos -> Map -> Map
moveBox _pos1 _pos2 _map = map (map change) _map where
    change tile =
        let
            pos    = posTile tile
            _item1 = getItemByPos _pos1 _map
            _item2 = getItemByPos _pos2 _map
            (_item1', _item2') = case () of _
                                                | _item1 == Just Box     && _item2 == Just Slot -> (Nothing,   Just SlotBox)
                                                | _item1 == Just SlotBox && _item2 == Just Slot -> (Just Slot, Just SlotBox)
                                                | _item1 == Just SlotBox && isNothing _item2 -> (Just Slot, Just Box)
                                                | otherwise -> (_item2, _item1)
        in
                if isPosEquals pos _pos1 then Tile _pos1 _item1'
           else if isPosEquals pos _pos2 then Tile _pos2 _item2'
           else tile

drawMap :: LevelEntry -> String
drawMap _level = let _map = mapLevelEntry _level
                     _pos = posLevelEntry _level in
                     unlines . map (map (tileToChar _pos)) $ _map

levelDatabase :: [LevelEntry]
levelDatabase = [
        LevelEntry (5, 4) (strToMap [
            "..XXX.....",
            "..XºX.....",
            "..XbXXXX..",
            "XXX  bºX..",
            "Xºb  XXX..",
            "XXXXbX....",
            "...XºX....",
            "...XXX...."
        ]),
        LevelEntry (2, 2) (strToMap [
            "XXXXXXXXXX",
            "X        X",
            "X º      X",
            "X º    º X",
            "X b º    X",
            "X     b  X",
            "X        X",
            "XXXXXXXXXX"
        ])
    ]


