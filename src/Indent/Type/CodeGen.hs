{-# LANGUAGE ViewPatterns #-}

module Indent.Type.CodeGen (
      CodeGen
    ) where


import Control.Arrow
import Control.Monad.ListM
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.Tagged
import Indent.Class


data CodeGen = St {
      indentAmount :: Int
    }


initState :: CodeGen
initState = St {
      indentAmount = 0
    }


instance Indenter CodeGen where
    indent = Tagged . unlines . flip evalState initState . mapM (tabify . dropWs) . lines
        where
            tabify str = do
                n <- tabAmount str
                return $ replicate n '\t' ++ str


dropWs :: String -> String
dropWs = dropWhile isSpace


tabAmount :: String -> State CodeGen Int
tabAmount (stripPrefix "DEFINE" -> Just (stripPrefix "(" . dropWs -> Just rest)) = tabAmount rest
tabAmount (stripPrefix "HC_Open_" -> Just _) = fmap (subtract 1) $ modifyAmount (+ 1)
tabAmount (stripPrefix "HC_KOpen_" -> Just _) = fmap (subtract 1) $ modifyAmount (+ 1)
tabAmount (stripPrefix "HC_Close_" -> Just _) = modifyAmount (subtract 1)
tabAmount (stripPrefix "HC_Begin_" -> Just _) = fmap (subtract 1) $ modifyAmount (+ 1)
tabAmount (stripPrefix "HC_End_" -> Just _) = modifyAmount (subtract 1)
tabAmount _ = gets indentAmount


modifyAmount :: (Int -> Int) -> State CodeGen Int
modifyAmount f = do
    modify $ \st -> st { indentAmount = f $ indentAmount st }
    gets indentAmount







