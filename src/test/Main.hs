module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10000} Spec.spec

