module Lib where

import Grammar
import Parse
import Pos
import qualified TrackDefinitions

runOnFiles :: [String] -> IO ()
runOnFiles = mapM_ runOnFile

unwrap (Left err) = error err
unwrap (Right x) = x

runOnFile :: String -> IO ()
runOnFile file = do
  contents <- readFile file
  let program = unwrap $ parseProgram file contents
  putStrLn . show $ program
  putStrLn . show $ TrackDefinitions.run program
