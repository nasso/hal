module Lib (standardContext) where

import qualified Data.Map.Strict as Map
import Eval

standardContext :: Context
standardContext = fromScope $ Map.fromList builtins

builtins :: [(String, Value)]
builtins =
  []
