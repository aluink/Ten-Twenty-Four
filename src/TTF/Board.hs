
module TTF.Board (
  Board,
  Value,
  Move(..),

  newBoard,
  makeMove,
  printBoard
) where

import System.Random

data Board = Board {
  positions :: [Value],
  state :: StdGen
}

data Move = RIGHT | LEFT | UP | DOWN deriving (Show,Eq)

data Value = EMPTY | ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN deriving (Eq,Ord)

instance Show Value where
  show v | v == ONE = " 2  "
         | v == TWO = " 4  "
         | v == THREE = " 8  "
         | v == FOUR = " 16 "
         | v == FIVE = " 32 "
         | v == SIX = " 64 "
         | v == SEVEN = "128 "
         | v == EIGHT = "256 "
         | v == NINE = "512 "
         | v == TEN = "1024"
         | otherwise = "    "


printBoard :: Board -> IO ()
printBoard (Board p _) = do
  putStrLn "+----+----+----+----+"
  putStrLn ("|" ++ (s 0) ++ "|" ++ (s 1) ++ "|" ++ (s 2) ++ "|" ++ (s 3) ++ "|")
  putStrLn "+----+----+----+----+"
  putStrLn ("|" ++ (s 4) ++ "|" ++ (s 5) ++ "|" ++ (s 6) ++ "|" ++ (s 7) ++ "|")
  putStrLn "+----+----+----+----+"
  putStrLn ("|" ++ (s 8) ++ "|" ++ (s 9) ++ "|" ++ (s 10) ++ "|" ++ (s 11) ++ "|")
  putStrLn "+----+----+----+----+"
  putStrLn ("|" ++ (s 12) ++ "|" ++ (s 13) ++ "|" ++ (s 14) ++ "|" ++ (s 15) ++ "|")
  putStrLn "+----+----+----+----+"
  where
    s x = show $ p!!x

makeMove :: Board -> Move -> Board
makeMove (Board p s) _ = Board np ns'
  where
    (i, ns) = next s
    (i',ns') = next ns
    f (x,EMPTY) vs = vs
    f (k,_) vs = k:vs
    emptySpaces = foldr f [] $ zip [0..] p
    newPieceL = emptySpaces!!(i `mod` (length emptySpaces))
    np = take newPieceL p ++ [(if even i' then ONE else TWO)] ++ (drop (newPieceL+1) p)

newBoardRand :: Int -> Board
newBoardRand i = Board (take 16 $ repeat EMPTY) $ mkStdGen i

newBoard :: Board
newBoard = newBoardRand 0
