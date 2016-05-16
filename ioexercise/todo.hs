import System.IO
import Data.Char
import System.Directory
import Data.List

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempFile, tempHandle) <- openTempFile "." "tmp"
  content <- hGetContents handle
  let todoTasks = lines content
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items:" 
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete? "
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempFile "todo.txt"