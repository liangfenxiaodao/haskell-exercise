import System.IO
import Data.Char
import System.Directory
import Data.List
--main = do
--  handle <- openFile "girlfriend.txt" ReadMode
--  content <- hGetContents handle
--  putStrLn content
--  hClose handle

--main = do
--  withFile "girlfriend.txt" ReadMode (\handle -> do
--    content <- hGetContents handle
--    putStrLn content)

main = do
  content <- readFile "girlfriend.txt"
  writeFile "girlfriendCap.txt" (map toUpper content)