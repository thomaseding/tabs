module Text.Indent.Class (
      Indenter(..)
    , IndentMode(..)
    , Tagged
    ) where


import Data.Tagged


data IndentMode = DropOldTabs | KeepOldTabs


class Indenter a where
    indent :: IndentMode -> String -> Tagged a String


