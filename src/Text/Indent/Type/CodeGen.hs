{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Indent.Type.CodeGen (
    CodeGen
) where


import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.Maybe
import Data.Tagged
import Text.Indent.Class


data CodeGen


type IndentAmount = Int
type Line = String


data IndentItem
    = Open
    | Begin
    | Brace
    deriving (Show, Eq)


data MalformedStack = MalformedStack


data IndentState = IndentState {
    indentAmount :: IndentAmount,
    itemStack :: Either MalformedStack [IndentItem]
}


initState :: IndentState
initState = IndentState {
    indentAmount = 0,
    itemStack = Right [] }


instance Indenter CodeGen where
    indent mode = Tagged . unlines . flip evalState initState . mapM (tabify . wsOp) . lines
        where
            wsOp = case mode of
                DropOldTabs -> dropWs
                KeepOldTabs -> id
            tabify line = do
                newTabAmount <- calculateTabs $ dropWs line
                return $ replicate newTabAmount '\t' ++ line


dropWs :: String -> String
dropWs = dropWhile isSpace


lastNonWs :: String -> Maybe Char
lastNonWs s = case dropWs $ reverse s of
    [] -> Nothing
    c : _ -> Just c


calculateTabs :: Line -> State IndentState IndentAmount
calculateTabs line = case line of
    (stripPrefix "DEFINE" -> Just (stripPrefix "(" . dropWs -> Just rest)) -> calculateTabs rest
    (isPrefixOf "HC_Open_" -> True) -> do
        n <- gets indentAmount
        push Open
        return n
    (isPrefixOf "HC_KOpen_" -> True) -> do
        n <- gets indentAmount
        push Open
        return n
    (isPrefixOf "HC_Close_" -> True) -> do
        pop Open
        gets indentAmount
    (isPrefixOf "HC_Begin_" -> True) -> do
        n <- gets indentAmount
        push Begin
        return n
    (isPrefixOf "HC_End_" -> True) -> do
        pop Begin
        gets indentAmount
    (lastNonWs -> Just '{') -> do
        n <- gets indentAmount
        push Brace
        return n
    (isPrefixOf "}" -> True) -> do
        popTill Brace
        pop Brace
        gets indentAmount
    _ -> gets indentAmount


topItem :: State IndentState (Maybe IndentItem)
topItem = gets $ either (const Nothing) listToMaybe . itemStack


push :: IndentItem -> State IndentState ()
push item = do
    modify $ \st -> st { itemStack = fmap (item :) $ itemStack st }
    modify $ \st -> st { indentAmount = indentAmount st + 1 }


pop :: IndentItem -> State IndentState ()
pop item = do
    mItem <- topItem
    if Just item == mItem
        then do
            modify $ \st -> st { itemStack = fmap tail $ itemStack st }
            modify $ \st -> st { indentAmount = indentAmount st - 1 }
        else modify $ \st -> st { itemStack = Left MalformedStack }


popTill :: IndentItem -> State IndentState ()
popTill destItem = do
    mItem <- topItem
    case mItem of
        Nothing -> return ()
        Just item -> if item == destItem
            then return ()
            else do
                pop item
                popTill destItem











