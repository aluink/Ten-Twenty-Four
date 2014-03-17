import System.IO
import TTF.Board
import Data.Char


main :: IO ()
main = go newBoard

go :: Board -> IO ()
go b = do
  m <- scanline
  let b' = makeMove b undefined
  printBoard b'
  go b'
            
           

 
scanline :: IO (Maybe Move)
scanline = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  c <- hGetChar stdin
  if ord c == 27 then do
    c <- hGetChar stdin
    if ord c == 91 then do
      c <- hGetChar stdin
      case ord c of
         65 -> return $ Just UP
         66 -> return $ Just DOWN
         67 -> return $ Just RIGHT
         68 -> return $ Just LEFT
     else return Nothing
  else return Nothing
