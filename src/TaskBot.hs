module TaskBot
  ( runBot
  ) where

import Consts
import System.IO

type Task = String
type Name = String

-- | Run task bot.
-- Commands:
-- /list -- show task list
-- /complete -- complete the last task
-- /exit -- stop bot
-- Any other input is considered as new task.
runBot :: IO ()
runBot = do
  -- disable buffering for stdout
  hSetBuffering stdout NoBuffering
  putStrLn namePrompt
  name <- getLine
  putStrLn botNamePrompt
  botName <- getLine
  go name botName []
  where
    -- Helper function to interact with user and update tasks list
    go :: Name -> Name -> [Task] -> IO ()
    go name botName taskList = do
      putStr $ name ++ "> "
      str <- getLine
      if (str == "/exit")
        then putStrLn goodbyeMsg
        else do
          -- process input unless it is an "/exit" command
          let (output, newTaskList) = processCommand botName str taskList
          putStrLn (botName ++ botPrompt ++ output)
          go name botName newTaskList

-- | Process user input. Returns output string to be printed by bot and
-- updated list of tasks in a tuple.
processCommand :: Name -> String -> [Task] -> (String, [Task])
processCommand botName cmd prevTaskList = case cmd of
  "/list" -> cmdList prevTaskList
  "/complete" -> cmdComplete botName prevTaskList
  "/remove" -> cmdRemove prevTaskList
  _ -> addTask cmd prevTaskList

-- | Convert a list of strings to a numbered string.
showNumbered :: Int -> [Task] -> String
showNumbered _ [] = ""
showNumbered i (x: xs) = (show (i + 1)) ++ ". " ++ x ++ "\n" ++ (showNumbered (i + 1) xs)

-- | Command to show tasks list.
cmdList :: [Task] -> (String, [Task])
cmdList tasks = (showNumbered 0 tasks, tasks)

-- | Command to complete the last task.
cmdComplete :: Name -> [Task] -> (String, [Task])
cmdComplete botName [] = (botName ++ noTasksMsg, [])
cmdComplete _ (_:xs) = (completeMsg, xs)

-- | Add new task to tasks list.
addTask :: String -> [Task] -> (String, [Task])
addTask task l = (newTaskMsg, task:l)

-- | Remove all tasks from the list.
cmdRemove :: [Task] -> (String, [Task])
cmdRemove _ = (removeMsg, [])