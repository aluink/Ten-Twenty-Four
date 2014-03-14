import System.IO
import TTF.Board
import Data.Char


main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    scanline
  where
            
            
