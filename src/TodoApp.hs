{-# LANGUAGE DeriveGeneric #-}

module TodoApp where

import qualified Lib as L
import Options.Applicative
import Options.Generic

program :: IO ()
program = do
  -- todoInput <- execParser todoInputParserInfo
  todoInput <- getRecord "helper message"
  run todoInput

run :: TodoInput -> IO ()
run t = do
  let v = L.checkTodo (title t) (dueDate t) 0
  putStrLn $ show t
  putStrLn $ render v

render :: Show p => Either L.TodoValidationError (L.Todo String p) -> String
render (Left L.TitleEmpty) = "please insert a non-empty title"
render (Left L.PastDueDate) = "due date has to be in the future, if present"
render (Right v) = "valid Todo " ++ (show v)

-- manual applicative parser
todoInputParser :: Parser TodoInput
todoInputParser =
  TodoInput
    <$> strOption (long "title")
    <*> optional (option auto (long "dueDate"))
    <*> switch (long "verbose")

todoInputParserInfo :: ParserInfo TodoInput
todoInputParserInfo = info todoInputParser fullDesc

data TodoInput = TodoInput
  { title :: String,
    dueDate :: Maybe Int,
    verbose :: Bool
  }
  deriving (Generic, Eq, Show)

instance ParseRecord TodoInput
