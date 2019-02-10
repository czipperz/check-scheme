module Lib (runOnFiles, runOnFile) where

import Parse
import qualified TrackDefinitions

runOnFiles :: [String] -> IO ()
runOnFiles = mapM_ runOnFile

unwrap :: Either String a -> a
unwrap (Left err) = error err
unwrap (Right x) = x

runOnFile :: String -> IO ()
runOnFile file = do
  contents <- readFile file
  let program = unwrap $ parseProgram file contents
  putStrLn . show $ program
  putStrLn . show $ TrackDefinitions.run program
