
module TTF.Board (
  Board,
  Value,

  newBoard,
  printBoard
) where

import System.Random

data Board = Board {
  positions :: [Value],
  state :: StdGen
}

data Move = RIGHT | LEFT | UP | DOWN

data Value = EMPTY | ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN deriving (Eq,Ord)

instance Show Value where
  show v | v == ONE = " 2  "
         | v == TWO = " 4  "
         | v == TWO = " 8  "
         | v == TWO = " 16 "
         | v == TWO = " 32 "
         | v == TWO = " 64 "
         | v == TWO = "128 "
         | v == TWO = "256 "
         | v == TWO = "512 "
         | v == TWO = "1024"
         | otherwise = "    "

scanline :: IO (Maybe Move)
scanline = do
  c <- hGetChar stdin
  input <- if ord c == 27 then do
             c <- hGetChar stdin
             if ord c == 91 then do
               c <- hGetChar stdin
               case ord c of
                  65 -> return "UP"
                  66 -> return "DOWN"
                  67 -> return "RIGHT"
                  68 -> return "LEFT"
              else return ("BAD2: " ++ show (ord c))
           else return ("BAD1: " ++ show (ord c))
  return Just input

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

newBoard :: Board
newBoard = Board (take 16 $ repeat EMPTY) $ mkStdGen 0
