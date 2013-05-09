module Indent (
      main
    ) where


import Data.Char (isSpace, toLower)
import Data.List (partition)
import Data.Tagged
import Indent.Class
import Indent.Type.CodeGen
import System.Environment (getArgs)
import System.FilePath (takeExtension)


main :: IO ()
main = do
  args <- getArgs
  let (opts, files) = partition ((== "-") . take 1) args
  if any (`elem` ["-h", "--help"]) opts || null files
    then help
    else mapM_ go files


help :: IO ()
help = mapM_ putStrLn [
    "Usage: tindent [options] files"
  , "Description: Indents files based on filetype."
  ]


type Indent a = String -> Tagged a String


go ::FilePath -> IO ()
go file = do
  str <- readFile file
  str' <- indentIO str
  length str `seq` writeFile file str'
  where
    indentIO = case map toLower $ drop 1 $ takeExtension file of
      "" -> return
      "c" -> return . untag . (indent :: Indent CodeGen)
      "cpp" -> return . untag . (indent :: Indent CodeGen)
      _ -> \str -> do
        putStrLn $ "Unknown filetype: Skipping " ++ file
        return str


