module Indent.Class (
      Indenter(..)
    , Tagged
    ) where


import Data.Tagged


class Indenter a where
    indent :: String -> Tagged a String


