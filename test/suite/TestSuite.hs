module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Hazard.Tests.Core

main :: IO ()
main = defaultMain tests
  where tests = [testGroup "Hazard Core" Hazard.Tests.Core.tests
                ]
