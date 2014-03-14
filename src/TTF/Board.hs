
module TTF.Board (
  Board,
  Value,

  value,
  unValue
) where

data Board = Board {
  positions :: Value
}

data Value = Value_ Int

instance Eq Value where
  (==) (Value_ i) (Value_ j) = i == j

instance Ord Value where
  compare a b = compare (unValue a) (unValue b)


value :: Int -> Maybe Value
value i | i < 11 && i > 0 = Just $ Value_ i
        | otherwise       = Nothing


unValue :: Value -> Int
unValue (Value_ i) = i
