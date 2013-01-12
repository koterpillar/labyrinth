import Data.List

data Cell = Land
    deriving (Eq)

instance Show Cell where
    show Land = "."

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

showH :: Wall -> String
showH NoWall   = " "
showH Wall     = "-"
showH HardWall = "="

showV :: Wall -> String
showV NoWall   = " "
showV Wall     = "|"
showV HardWall = "‖"

data Player = Player { bullets  :: Int
                     , grenades :: Int
                     , treasure :: Maybe Treasure
                     }
              deriving (Eq)

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq)

-- wallsV and wallsH are considered to be to the left and top of the cells
data Labyrinth = Labyrinth { cells   :: [[Cell]]
                           , wallsH  :: [[Wall]]
                           , wallsV  :: [[Wall]]
                           , players :: [Player]
                           }
                 deriving (Eq)

labWidth :: Labyrinth -> Int
labWidth = length . cells

labHeight :: Labyrinth -> Int
labHeight = length . head . cells

cell :: Labyrinth -> Int -> Int -> Cell
cell = undefined

wallH :: Labyrinth -> Int -> Int -> Wall
wallH = undefined

wallV :: Labyrinth -> Int -> Int -> Wall
wallV = undefined

showWallLine :: Labyrinth -> Int -> String
showWallLine l y = mk ++ intercalate mk ws ++ mk
    where mk = "*"
          ws = map (\x -> showH $ wallH l x y) [1..w]
          w  = labWidth l

showCellLine :: Labyrinth -> Int -> String
showCellLine = undefined

instance Show Labyrinth where
    show l = intercalate "\n" $ firstLines ++ [lastLine]
        where h = labHeight l
              showLine l i = [showWallLine l i, showCellLine l i]
              firstLines = concat $ map (showLine l) [0..h - 1]
              lastLine = showWallLine l h
