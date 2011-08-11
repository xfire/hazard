module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Hazard.Tests

main :: IO ()
main = defaultMain tests
  where tests = [testGroup "Hazard Dummy Tests" Hazard.Tests.tests
                ]
