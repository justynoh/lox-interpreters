module Types.Error where

data Error = 
  ScanError String String Int
  deriving Show
