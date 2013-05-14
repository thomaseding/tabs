module Tabs (
      main
    ) where


import Data.Char (isSpace, toLower)
import Data.List (partition)
import Data.Tagged
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO
import Text.Indent.Class
import Text.Indent.Type.CodeGen


main :: IO ()
main = do
  args <- getArgs
  let (opts, files) = partition ((== "-") . take 1) args
  if any (`elem` ["-h", "--help"]) opts
    then help
    else if null files
        then byStdin runIndent
        else mapM_ (byFile runIndent) files


help :: IO ()
help = mapM_ putStrLn [
    "Usage: myindent [options] files"
  , "Description: Indents files based on filetype."
  ]


data IndentType
    = HcCodeGen


byStdin :: (IndentType -> String -> String) -> IO ()
byStdin f = do
    contents <- getContents
    putStrLn $ f HcCodeGen contents


byFile :: (IndentType -> String -> String) -> FilePath -> IO ()
byFile f file = do
    contents <- readFile file
    mIndentType <- case map toLower $ drop 1 $ takeExtension file of
        "" -> return Nothing
        "c" -> return $ Just HcCodeGen
        "cpp" -> return $ Just HcCodeGen
        _ -> do
            hPutStrLn stderr $ "Uknown filetype: Skipping " ++ file
            return Nothing
    case mIndentType of
        Nothing -> return ()
        Just indentType -> do
            let newContents = f indentType contents
            length contents `seq` writeFile file newContents


type Indent a = String -> Tagged a String


runIndent :: IndentType -> String -> String
runIndent indentType = case indentType of
    HcCodeGen -> untag . (indent DropOldTabs :: Indent CodeGen)










