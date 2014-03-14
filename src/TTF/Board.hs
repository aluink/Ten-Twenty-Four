
module TTF.Board (
  Board,
  Value,

  value,
  newBoard,
  printBoard
) where

data Board = Board {
  positions :: [Value]
}

data Value = Value_ Int

instance Eq Value where
  (==) (Value_ i) (Value_ j) = i == j

instance Ord Value where
  compare a b = compare (unValue a) (unValue b)

printBoard :: Board -> IO ()
printBoard (Board p) = do
  putStrLn "+---+---+---+---+"
  putStrLn ("| " ++ (s 0) ++ " | " ++ (s 1) ++ " | " ++ (s 2) ++ " | " ++ (s 3) ++ " |")
  putStrLn "+---+---+---+---+"
  putStrLn ("| " ++ (s 4) ++ " | " ++ (s 5) ++ " | " ++ (s 6) ++ " | " ++ (s 7) ++ " |")
  putStrLn "+---+---+---+---+"
  putStrLn ("| " ++ (s 8) ++ " | " ++ (s 9) ++ " | " ++ (s 10) ++ " | " ++ (s 11) ++ " |")
  putStrLn "+---+---+---+---+"
  putStrLn ("| " ++ (s 12) ++ " | " ++ (s 13) ++ " | " ++ (s 14) ++ " | " ++ (s 15) ++ " |")
  putStrLn "+---+---+---+---+"
  where
    s x | (unValue (p!!x)) > 0 = show $ unValue (p!!x)
        | otherwise = " "

newBoard :: Board
newBoard = Board $ take 16 $ repeat (Value_ 0)

value :: Int -> Maybe Value
value i | i > 0 = Just $ Value_ i
        | otherwise       = Nothing

unValue :: Value -> Int
unValue (Value_ i) = i

combine :: Value -> Value -> Maybe Value
combine a@(Value_ i) b@(Value_ j) | a == b    = Just $ Value_ (i*2)
                                  | otherwise = Nothing
